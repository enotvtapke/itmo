<template>
    <div class="middle">
        <Sidebar :posts="viewPosts.slice(0, 2)"/>
        <main>
            <Index v-if="page === 'Index'"/>
            <Enter v-if="page === 'Enter'"/>
            <WritePost v-if="page === 'WritePost'"/>
            <EditPost v-if="page === 'EditPost'"/>
            <Register v-if="page === 'Register'"/>
            <Users v-if="page === 'Users'" :users="users"/>
            <Posts v-if="page === 'Posts'" :posts="viewPosts"/>
            <Post v-if="page === 'Post'" :post="args.post"/>
        </main>
    </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./page/Index";
import Enter from "./page/Enter";
import WritePost from "./page/WritePost";
import EditPost from "./page/EditPost";
import Register from "./page/Register";
import Users from "./page/Users";
import Posts from "./page/Posts";
import Post from "./page/Post";

export default {
    name: "Middle",
    data: function () {
        return {
            page: "Index",
            args: null
        }
    },
    components: {
        Register,
        WritePost,
        Enter,
        Index,
        Sidebar,
        EditPost,
        Users,
        Posts,
        Post
    },
    props: ["users", "posts", "comments"],
    computed: {
        viewPosts: function () {
            let posts = this.posts;
            for (const key in posts) {
                posts[key].user = this.users[posts[key].userId];
                posts[key].comments = []
            }
            const comments = this.comments;
            for (const key in comments) {
                comments[key].user = this.users[comments[key].userId];
            }
            for (const key in this.comments) {
                const comment = this.comments[key];
                posts[comment.postId].comments.push(comment);
            }
            return Object.values(posts).sort((a, b) => b.id - a.id);
        }
    },
    beforeCreate() {
        this.$root.$on("onChangePage", (page, args = null) => {
            this.page = page;
            this.args = args;
        })
    }
}
</script>

<style scoped>

</style>
