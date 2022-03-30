{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Database.Setup where

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

setupDB :: Connection -> IO ()
setupDB conn = do
  let create =
        Session.sql
          [TH.uncheckedSql|
            CREATE OR REPLACE FUNCTION update_updated_at()   
            RETURNS TRIGGER AS $$
            BEGIN
                NEW.updated_at = now();
                RETURN NEW;   
            END;
            $$ language 'plpgsql';
            
            create table if not exists users (
                pk_user serial primary key,
                username text unique not null,
                email text unique not null,
                image text not null default 'https://api.realworld.io/images/smiley-cyrus.jpeg',
                bio text not null default '',
                hash text not null,
                created_at timestamp with time zone default current_timestamp,
                updated_at timestamp with time zone default current_timestamp
            );

            create trigger update_users before update on users for each row execute procedure update_updated_at();

            create table if not exists follows (
                follower_id serial references users(pk_user) on delete cascade,
                followee_id serial references users(pk_user) on delete cascade,
                constraint pk_follows primary key (follower_id, followee_id)
            );

            create table if not exists articles (
                pk_article serial primary key,
                fk_user serial references users(pk_user) on delete cascade,
                title text unique not null,
                slug text unique not null,
                description text unique not null,
                body text unique not null,
                created_at timestamp with time zone default current_timestamp,
                updated_at timestamp with time zone default current_timestamp
            );

            create trigger update_articles before update on articles for each row execute procedure update_updated_at();

            create table if not exists comments (
                pk_comment serial primary key,
                fk_user serial references users(pk_user) on delete cascade,
                fk_article serial references articles(pk_article) on delete cascade,
                body text unique not null,
                created_at timestamp with time zone default current_timestamp,
                updated_at timestamp with time zone default current_timestamp
            );

            create trigger update_comments before update on comments for each row execute procedure update_updated_at();

            create table if not exists favorites (
                fk_user serial references users(pk_user) on delete cascade,
                fk_article serial references articles(pk_article) on delete cascade,
                constraint pk_favorites primary key (fk_user, fk_article)
            );

            create table if not exists tags (
                tag text not null,
                fk_article serial references articles(pk_article) on delete cascade,
                constraint pk_tags primary key (tag, fk_article)
            );
        |]
      destroy =
        Session.sql
          [TH.uncheckedSql|
            drop table if exists users, articles, comments, favorites, tags, follows cascade;
            drop trigger if exists update_updated_at on users;
            drop trigger if exists update_updated_at on articles;
            drop trigger if exists update_updated_at on comments;
            drop function if exists update_update_at;
        |]

  result <- Session.run destroy conn
  print "Destroying tables..."
  print result
  print "Making tables..."
  result <- Session.run create conn
  print result
