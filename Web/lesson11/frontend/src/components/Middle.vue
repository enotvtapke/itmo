<template>
    <div class="middle">
        <Sidebar :posts="Object.fromEntries(Object.entries(viewPosts).slice(0, 5))"/>
        <main>
            <Index v-if="page === 'Index'" :posts="viewPosts"/>
            <Enter v-if="page === 'Enter'"/>
            <Register v-if="page === 'Register'"/>
            <Users v-if="page === 'Users'" :users="users"/>
            <WritePost v-if="page === 'WritePost'"/>
            <Post v-if="page === 'Post'" :post="args.post"/>
        </main>
    </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./main/Index";
import Enter from "./main/Enter";
import Register from "./main/Register";
import Users from "./main/Users";
import WritePost from "./main/WritePost";
import Post from "./main/Post";

export default {
    name: "Middle",
    data: function () {
        return {
            page: "Index",
            args: null
        }
    },
    components: {
        Post,
        WritePost,
        Register,
        Enter,
        Index,
        Sidebar,
        Users
    },
    props: ["posts", "users"],
    computed: {
        viewPosts: function () {
            return Object.values(this.posts).sort((a, b) => b.id - a.id);
        }
    }, beforeCreate() {
        this.$root.$on("onChangePage", (page, args = null) => {
            this.page = page;
            this.args = args;
        })
    }
}
</script>

<style scoped>

</style>
