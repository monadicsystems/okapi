import { Tag } from "@markdoc/markdoc";

export const endpoint_tester = {
  render: 'endpoint-tester',
  children: [],
  attributes: {
    baseURL: {
      type: String,
      default: 'http://localhost:3000',
      errorLevel: 'critical'
    }
  },
  selfClosing: true,
  transform: (node, config) => {
    const attributes = node.transformAttributes(config);

    const methodDropdown = new Tag('select', { 'name': 'methods', 'class': 'rounded-md py-2 mr-1' }, [
      new Tag('option', { 'value': 'get' }, ["GET"]),
      new Tag('option', { 'value': 'post' }, ["POST"]),
      new Tag('option', { 'value': 'put' }, ["PUT"]),
      new Tag('option', { 'value': 'delete' }, ["DELETE"]),
    ]);

    const urlInput = new Tag('input', { 'type': 'text', 'name': 'url', 'class': 'rounded-md mr-4' }, []);

    const submitButton = new Tag('button', {
      'class': 'rounded-md bg-leaf-100 py-2 px-4 text-sm font-semibold text-slate-900 hover:bg-leaf-50 focus:outline-none focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-leaf-50/50 active:bg-leaf-100'
    }, ['Send Request'])

    return new Tag('div', { 'class': "flex flex-row w-full justify-center" }, [
      methodDropdown,
      urlInput,
      submitButton
    ]);
  }
};
