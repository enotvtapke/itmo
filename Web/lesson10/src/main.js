import Vue from 'vue'
import App from './App.vue'
import data from "./data.js";

Vue.config.productionTip = false

new Vue({
  data: function () {
    return data;
  },
  methods: {
    changePage: function (page, args) {
      this.$root.$emit("onChangePage", page, args);
    }
  },
  render: h => h(App),
}).$mount('#app')
