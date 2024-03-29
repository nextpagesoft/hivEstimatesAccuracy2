import React from 'react';
import ReactDOMClient from 'react-dom/client';
import { Provider } from 'mobx-react';
import $ from 'jquery';
import App from './App';
import AppManager from './stores/AppManager';

// Attach to window for shinyjs
window.jQuery = $;
window.$ = $;

const appMgr = new AppManager();
const container = document.getElementById('app');
const root = ReactDOMClient.createRoot(container);

root.render(
  <Provider appMgr={appMgr}>
    <React.StrictMode>
      <App/>
    </React.StrictMode>
  </Provider>
);

if (module.hot) {
  module.hot.accept();
}
