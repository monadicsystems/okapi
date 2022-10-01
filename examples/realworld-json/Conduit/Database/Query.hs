{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Database.Query where

import Conduit.Auth
import Conduit.Type
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Bifunctor (bimap)
import Data.Either (rights)
import Data.Function ((&))
import Data.Int
import Data.Profunctor (dimap)
import Data.Text
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session, statement)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import Hasql.TH (resultlessStatement)
import qualified Hasql.TH as TH

-- DB Functions

loginUser :: MonadHandler m => Login -> m (Either QueryError EncodedUser)
loginUser loginData = do
  result <- execSql (statement loginData sql)
  case result of
    Right user -> do
      secret <- grab @Text
      pure $ Right $ encodeUser secret user
    Left queryError -> pure $ Left queryError
  where
    sql :: Statement Login User
    sql =
      dimap
        loginToTuple
        tupleToUser
        [TH.singletonStatement|
          select
            pk_user :: int4,
            email :: text,
            username :: text,
            bio :: text,
            image :: text,
            created_at :: timestamptz,
            updated_at :: timestamptz
          from users
          where email = $1 :: text and hash = crypt($2 :: text, hash)
        |]

    loginToTuple :: Login -> (Text, Text)
    loginToTuple Login {..} = (loginEmail, loginPassword)

registerUser :: MonadHandler m => Register -> m (Either QueryError EncodedUser)
registerUser register = do
  result <- execSql (statement register sql)
  case result of
    Right user -> do
      secret <- grab @Text
      pure $ Right $ encodeUser secret user
    Left queryError -> pure $ Left $ queryError
  where
    sql :: Statement Register User
    sql =
      dimap
        registerToTuple
        tupleToUser
        [TH.singletonStatement|
          insert into users (email, username, hash)
          values ($1 :: text, $2 :: text, crypt($3 :: text, gen_salt('bf')))
          returning pk_user :: int4, email :: text, username :: text, bio :: text, image :: text, created_at :: timestamptz, updated_at :: timestamptz
        |]

    registerToTuple :: Register -> (Text, Text, Text)
    registerToTuple Register {..} = (registerEmail, registerUsername, registerPassword)

tupleToUser :: (Int32, Text, Text, Text, Text, UTCTime, UTCTime) -> User
tupleToUser (userID, userEmail, userUsername, userBio, userImage, userCreatedAt, userUpdatedAt) = User {..}

getCurrentUser :: MonadHandler m => Int32 -> m (Either QueryError EncodedUser)
getCurrentUser userID = do
  result <- execSql (statement userID sql)
  case result of
    Right user -> do
      secret <- grab @Text
      pure $ Right $ encodeUser secret user
    Left queryError -> pure $ Left $ queryError
  where
    sql :: Statement Int32 User
    sql =
      dimap
        id
        tupleToUser
        [TH.singletonStatement|
          select pk_user :: int, email :: text, username :: text, bio :: text, image :: text, created_at :: timestamptz, updated_at :: timestamptz
          from users
          where $1 :: int4 = pk_user
        |]

updateUser :: MonadHandler m => Int32 -> UpdateUser -> m (Either QueryError EncodedUser)
updateUser userID updateUser = do
  result <- execSql (statement (userID, updateUser) sql)
  case result of
    Right user -> do
      secret <- grab @Text
      pure $ Right $ encodeUser secret user
    Left queryError -> pure $ Left $ queryError
  where
    sql :: Statement (Int32, UpdateUser) User
    sql =
      dimap
        userIDWithUpdateUserToTuple
        tupleToUser
        [TH.singletonStatement|
          update users
          set
            email = coalesce($2 :: text?, email),
            username = coalesce($3 :: text?, username),
            bio = coalesce($5 :: text?, bio),
            image = coalesce($6 :: text?, image),
            hash =
              case
                when $4 :: text? is null then hash
                else crypt($4 :: text?, gen_salt('bf'))
              end
          where pk_user = $1 :: int4
          returning pk_user :: int4, username :: text, email :: text, bio :: text, image :: text, created_at :: timestamptz, updated_at :: timestamptz
        |]

    userIDWithUpdateUserToTuple :: (Int32, UpdateUser) -> (Int32, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
    userIDWithUpdateUserToTuple (userID, UpdateUser {..}) = (userID, updateUserEmail, updateUserUsername, updateUserPassword, updateUserBio, updateUserImage)

getProfile :: MonadHandler m => Maybe Int32 -> Text -> m (Either QueryError Profile)
getProfile mbUserID username = execSql (statement (mbUserID, username) sql)
  where
    sql :: Statement (Maybe Int32, Text) Profile
    sql =
      dimap
        id
        tupleToProfile
        [TH.singletonStatement|
          select
            username :: text
            , bio :: text
            , image :: text
            , ( exists
                  ( select follower_id :: int4
                    from follows
                    inner join users on username = $2 :: text
                    where follower_id = $1 :: int4?
                  )
              ) :: bool
          from users
          where $2 :: text = username
        |]

tupleToProfile :: (Text, Text, Text, Bool) -> Profile
tupleToProfile (profileUsername, profileImage, profileBio, profileFollowing) = Profile {..}

followUser :: MonadHandler m => Int32 -> Text -> m (Either QueryError Profile)
followUser userID username = do
  execSql (statement (userID, username) sql)
  getProfile (Just userID) username
  where
    sql :: Statement (Int32, Text) ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          insert into follows (follower_id, followee_id)
          values ($1 :: int4, (select pk_user from users where users.username = $2 :: text))
        |]

unfollowUser :: MonadHandler m => Int32 -> Text -> m (Either QueryError Profile)
unfollowUser userID username = do
  execSql (statement (userID, username) sql)
  getProfile (Just userID) username
  where
    sql :: Statement (Int32, Text) ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          delete from follows
          where
            $1 :: int4 = follower_id and (select pk_user from users where users.username = $2 :: text) = followee_id
        |]

getArticles :: MonadHandler m => Maybe Int32 -> ArticlesQuery -> m (Either QueryError Articles)
getArticles mbUserID articlesQuery = execSql (statement (mbUserID, articlesQuery) sql)
  where
    sql :: Statement (Maybe Int32, ArticlesQuery) Articles
    sql =
      dimap
        articlesQueryToTuple
        (articleListToArticles . Vector.toList . fmap tupleToArticle)
        [TH.vectorStatement|
          select
            articles.slug :: text
            , articles.title :: text
            , articles.description :: text
            , articles.body :: text
            , (array(select tag from tags where tags.fk_article = articles.pk_article order by tag)) :: text[]
            , (exists(select favorites.fk_user from favorites where favorites.fk_user = $1 :: int4? and favorites.fk_article = articles.pk_article)) :: bool
            , (select count(*) from favorites favoritess where favoritess.fk_article = articles.pk_article) :: int4
            , articles.created_at :: timestamptz
            , articles.updated_at :: timestamptz
            , users.username :: text
            , users.bio :: text
            , users.image :: text
            , ( exists
                ( select follower_id :: int4
                  from follows
                  inner join users userss on userss.username = users.username
                  where follower_id = $1 :: int4?
                )
              ) :: bool
          from articles
          inner join users on articles.fk_user = users.pk_user
          where
            ($2 :: text? is null or (articles.pk_article in (select tags.fk_article from tags where tags.tag = $2 :: text? and tags.fk_article = articles.pk_article)))
            and ($3 :: text? is null or $3 :: text? = users.username)
            and ($4 :: text? is null or articles.pk_article in (select favoritesss.fk_article from favorites favoritesss inner join users userss on userss.username = $4 :: text? where favoritesss.fk_article = articles.pk_article))
          order by articles.created_at
          limit $5 :: int4
          offset $6 :: int4
        |]

    articlesQueryToTuple :: (Maybe Int32, ArticlesQuery) -> (Maybe Int32, Maybe Text, Maybe Text, Maybe Text, Int32, Int32)
    articlesQueryToTuple (mbUserID, ArticlesQuery {..}) =
      ( mbUserID,
        articlesQueryTag,
        articlesQueryAuthor,
        articlesQueryFavorited,
        articlesQueryLimit,
        articlesQueryOffset
      )

articleListToArticles :: [Article] -> Articles
articleListToArticles articlesArticleList = let articlesArticlesCount = Prelude.length articlesArticleList in Articles {..}

feedArticles :: MonadHandler m => Int32 -> Int32 -> Int32 -> m (Either QueryError Articles)
feedArticles userID limit offset = execSql (statement (userID, limit, offset) sql)
  where
    sql :: Statement (Int32, Int32, Int32) Articles
    sql =
      dimap
        id
        (articleListToArticles . Vector.toList . fmap tupleToArticle)
        [TH.vectorStatement|
          select
            slug :: text
            , title :: text
            , description :: text
            , body :: text
            , (array(select tag from tags where tags.fk_article = articles.pk_article)) :: text[]
            , true :: bool
            , (select count(*) from favorites where favorites.fk_article = articles.pk_article) :: int4
            , articles.created_at :: timestamptz
            , articles.updated_at :: timestamptz
            , users.username :: text
            , users.bio :: text
            , users.image :: text
            , ( exists
                ( select follower_id :: int4
                  from follows
                  inner join users userss on userss.username = users.username
                  where follower_id = $1 :: int4
                )
              ) :: bool
          from articles
          inner join users on articles.fk_user = users.pk_user
          inner join favorites favoritess on articles.pk_article = favoritess.fk_article and favoritess.fk_user = $1 :: int4
          order by articles.created_at
          limit $2 :: int4
          offset $3 :: int4
        |]

getArticle :: MonadHandler m => Text -> m (Either QueryError Article)
getArticle slug = execSql (statement slug sql)
  where
    sql :: Statement Text Article
    sql =
      dimap
        id
        tupleToArticle
        [TH.singletonStatement|
          select
            slug :: text
            , title :: text
            , description :: text
            , body :: text
            , (array(select tag from tags where tags.fk_article = articles.pk_article)) :: text[]
            , false :: bool
            , (select count(*) from favorites where favorites.fk_article = articles.pk_article) :: int4
            , articles.created_at :: timestamptz
            , articles.updated_at :: timestamptz
            , users.username :: text
            , users.bio :: text
            , users.image :: text
            , false :: bool
          from articles
          inner join users on articles.fk_user = users.pk_user
          where articles.slug = $1 :: text
        |]

getArticleByID :: MonadHandler m => Maybe Int32 -> Int32 -> m (Either QueryError Article)
getArticleByID mbUserID articleID = execSql (statement (mbUserID, articleID) sql)
  where
    sql :: Statement (Maybe Int32, Int32) Article
    sql =
      dimap
        id
        tupleToArticle
        [TH.singletonStatement|
          select
            slug :: text
            , title :: text
            , description :: text
            , body :: text
            , (array(select tag from tags where tags.fk_article = articles.pk_article)) :: text[]
            , (exists(select favorites.fk_user from favorites where favorites.fk_user = $1 :: int4? and favorites.fk_article = articles.pk_article)) :: bool
            , (select count(*) from favorites where favorites.fk_article = articles.pk_article) :: int4
            , articles.created_at :: timestamptz
            , articles.updated_at :: timestamptz
            , users.username :: text
            , users.bio :: text
            , users.image :: text
            , ( exists
                ( select follower_id :: int4
                  from follows
                  inner join users userss on userss.username = users.username
                  where follower_id = $1 :: int4?
                )
              ) :: bool
          from articles
          inner join users on articles.fk_user = users.pk_user
          where articles.pk_article = $2 :: int4
        |]

tupleToArticle ::
  ( Text,
    Text,
    Text,
    Text,
    Vector Text,
    Bool,
    Int32,
    UTCTime,
    UTCTime,
    Text,
    Text,
    Text,
    Bool
  ) ->
  Article
tupleToArticle
  ( articleSlug,
    articleTitle,
    articleDescription,
    articleBody,
    articleTagVector,
    articleFavorited,
    articleFavoritesCount,
    articleCreatedAt,
    articleUpdatedAt,
    authorUsername,
    authorBio,
    authorImage,
    authorFollowing
    ) =
    let articleTagList = Vector.toList articleTagVector
        articleAuthor = Author {..}
     in Article {..}

createTags :: MonadHandler m => Int32 -> Maybe [Text] -> m (Either QueryError ())
createTags articleID mbTags = case mbTags of
  Nothing -> pure $ Right ()
  Just tags -> execSql (statement (bimap Vector.fromList Vector.fromList $ unzip $ fmap (\tag -> (articleID, tag)) tags) sql)
  where
    sql :: Statement (Vector Int32, Vector Text) ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          insert into tags (fk_article, tag)
          select * from unnest ($1 :: int4[], $2 :: text[])
        |]

createArticle :: MonadHandler m => Int32 -> CreateArticle -> m (Either QueryError Article)
createArticle userID createArticleData = do
  eitherArticleID <- execSql (statement (userID, createArticleData) sql)
  case eitherArticleID of
    Left queryError -> pure $ Left queryError
    Right articleID -> do
      createTags articleID $ createArticleTagList createArticleData
      getArticleByID (Just userID) articleID
  where
    sql :: Statement (Int32, CreateArticle) Int32
    sql =
      dimap
        userIDWithCreateArticleToTuple
        id
        [TH.singletonStatement|
          insert into articles (fk_user, slug, title, description, body)
          values ($1 :: int4, $2 :: text, $3 :: text, $4 :: text, $5 :: text)
          returning pk_article :: int4
        |]

    userIDWithCreateArticleToTuple :: (Int32, CreateArticle) -> (Int32, Text, Text, Text, Text)
    userIDWithCreateArticleToTuple (userID, CreateArticle {..}) =
      (userID, slugify createArticleTitle, createArticleTitle, createArticleDescription, createArticleBody)

slugify :: Text -> Text
slugify = id

updateArticle :: MonadHandler m => Int32 -> Text -> UpdateArticle -> m (Either QueryError Article)
updateArticle userID slug updateArticleData = do
  eitherArticleID <- execSql (statement (userID, slug, updateArticleData) sql)
  case eitherArticleID of
    Left queryError -> pure $ Left queryError
    Right articleID -> getArticleByID (Just userID) articleID
  where
    sql :: Statement (Int32, Text, UpdateArticle) Int32
    sql =
      dimap
        userIDWithUpdateArticleToTuple
        id
        [TH.singletonStatement|
          update articles
          set
            title = coalesce($3 :: text?, title),
            slug = coalesce($4 :: text?, slug),
            description = coalesce($5 :: text?, description),
            body = coalesce($6 :: text?, body)
          where fk_user = $1 :: int4 and slug = $2 :: text
          returning pk_article :: int4
        |]

    userIDWithUpdateArticleToTuple :: (Int32, Text, UpdateArticle) -> (Int32, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
    userIDWithUpdateArticleToTuple (userID, slug, UpdateArticle {..}) =
      (userID, slug, slugify <$> updateArticleTitle, updateArticleTitle, updateArticleDescription, updateArticleBody)

deleteArticle :: MonadHandler m => Int32 -> Text -> m (Either QueryError ())
deleteArticle userID slug = execSql (statement (userID, slug) sql)
  where
    sql :: Statement (Int32, Text) ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          delete from articles
          where $1 :: int4 = fk_user and $2 :: text = slug
        |]

getCommentByID :: MonadHandler m => Int32 -> m (Either QueryError Comment)
getCommentByID commentID = execSql (statement commentID sql)
  where
    sql :: Statement Int32 Comment
    sql =
      dimap
        id
        tupleToComment
        [TH.singletonStatement|
          select
            pk_comment :: int4,
            comments.created_at :: timestamptz,
            comments.updated_at :: timestamptz,
            body :: text,
            users.username :: text,
            users.bio :: text,
            users.image :: text,
            false :: bool
          from comments
          inner join users on users.pk_user = comments.fk_user
          where $1 :: int4 = pk_comment
        |]

createComment :: MonadHandler m => Int32 -> Text -> CreateComment -> m (Either QueryError Comment)
createComment userID slug createCommentData = do
  eitherCommentID <- execSql (statement (userID, slug, createCommentData) sql)
  case eitherCommentID of
    Left queryError -> pure $ Left queryError
    Right commentID -> getCommentByID commentID
  where
    sql :: Statement (Int32, Text, CreateComment) Int32
    sql =
      dimap
        userIDAndSlugWithCreateCommentToTuple
        id
        [TH.singletonStatement|
          insert into comments (fk_user, fk_article, body)
          values ($1 :: int4, (select pk_article from articles where slug = $2 :: text), $3 :: text)
          returning pk_comment :: int4
        |]

    userIDAndSlugWithCreateCommentToTuple :: (Int32, Text, CreateComment) -> (Int32, Text, Text)
    userIDAndSlugWithCreateCommentToTuple (userID, slug, CreateComment {..}) =
      (userID, slug, createCommentBody)

tupleToComment :: (Int32, UTCTime, UTCTime, Text, Text, Text, Text, Bool) -> Comment
tupleToComment (commentID, commentCreatedAt, commentUpdatedAt, commentBody, authorUsername, authorBio, authorImage, authorFollowing) =
  let commentAuthor = Author {..}
   in Comment {..}

getComments :: MonadHandler m => Maybe Int32 -> Text -> m (Either QueryError Comments)
getComments mbUserID slug = execSql (statement (mbUserID, slug) sql)
  where
    sql :: Statement (Maybe Int32, Text) Comments
    sql =
      dimap
        id
        (Comments . Vector.toList . fmap tupleToComment)
        [TH.vectorStatement|
          select
            pk_comment :: int4,
            comments.created_at :: timestamptz,
            comments.updated_at :: timestamptz,
            comments.body :: text,
            users.username :: text,
            users.bio :: text,
            users.image :: text,
            ( exists
              ( select follower_id :: int4
                from follows
                inner join users userss on userss.username = users.username
                where follower_id = $1 :: int4?
              )
            ) :: bool
          from comments
          inner join users on users.pk_user = comments.fk_user
          inner join articles on articles.pk_article = comments.fk_article
          where $2 :: text = articles.slug
          order by comments.created_at
        |]

deleteComment :: MonadHandler m => Int32 -> Text -> Int32 -> m (Either QueryError ())
deleteComment userID _ commentID = execSql (statement (userID, commentID) sql)
  where
    sql :: Statement (Int32, Int32) ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          delete from comments
          where $1 :: int4 = fk_user and $2 :: int4 = pk_comment
        |]

favoriteArticle :: MonadHandler m => Int32 -> Text -> m (Either QueryError Article)
favoriteArticle userID slug = do
  eitherArticleID <- execSql (statement (userID, slug) sql)
  case eitherArticleID of
    Left queryError -> pure $ Left queryError
    Right articleID -> getArticleByID (Just userID) articleID
  where
    sql :: Statement (Int32, Text) Int32
    sql =
      dimap
        id
        id
        [TH.singletonStatement|
          insert into favorites (fk_user, fk_article)
          values ($1 :: int4, (select pk_article from articles where slug = $2 :: text))
          returning favorites.fk_article :: int4
        |]

unfavoriteArticle :: MonadHandler m => Int32 -> Text -> m (Either QueryError Article)
unfavoriteArticle userID slug = do
  eitherArticleID <- execSql (statement (userID, slug) sql)
  case eitherArticleID of
    Left queryError -> pure $ Left queryError
    Right articleID -> getArticleByID (Just userID) articleID
  where
    sql :: Statement (Int32, Text) Int32
    sql =
      dimap
        id
        id
        [TH.singletonStatement|
          delete from favorites
          using articles
          where favorites.fk_user = $1 :: int4 and articles.slug = $2 :: text and articles.pk_article = favorites.fk_article
          returning favorites.fk_article :: int4
        |]

getTags :: MonadHandler m => m (Either QueryError Tags)
getTags = execSql (statement () sql)
  where
    sql :: Statement () Tags
    sql =
      dimap
        id
        (Tags . Vector.toList)
        [TH.vectorStatement|
          select tag :: text from tags
        |]

deleteTags :: MonadHandler m => Int32 -> m (Either QueryError ())
deleteTags articleID = execSql (statement articleID sql)
  where
    sql :: Statement Int32 ()
    sql =
      dimap
        id
        id
        [TH.resultlessStatement|
          delete from tags where fk_article = $1 :: int4
        |]

execSql :: MonadHandler m => Session a -> m (Either QueryError a)
execSql session = do
  conn <- grab @Connection
  liftIO $ Session.run session conn
