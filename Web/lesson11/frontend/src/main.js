import Vue from 'vue'
import App from './App.vue'

Vue.config.productionTip = false

new Vue({
    methods: {
        changePage: function (page, args) {
            this.$root.$emit("onChangePage", page, args);
        }
    },
    render: h => h(App)
}).$mount('#app')
