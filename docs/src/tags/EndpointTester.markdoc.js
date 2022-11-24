export const endpoint_tester = {
  render: 'div',
  children: [],
  attributes: {
    baseURL: {
      type: String,
      default: 'http://localhost:3000',
      errorLevel: 'critical'
    }
  },
  selfClosing: true
};
