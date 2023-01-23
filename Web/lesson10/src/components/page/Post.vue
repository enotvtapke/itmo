<template>
    <div class="post-with-comments">
        <RawPost :post="post"/>
        <div class="comments">
            <Comment v-for="comment in post.comments" :comment="comment" :key="comment.id"/>
        </div>
    </div>
</template>

<script>
import RawPost from "./RawPost";
import Comment from "./Comment";

export default {
    name: "Post",
    components: {
        RawPost,
        Comment
    },
    props: ["post"],
    computed: {
        viewPost: function () {
            const viewPosts = this.posts;
            for (const key in viewPosts) {
                viewPosts[key].user = this.users[viewPosts[key].userId];
                viewPosts[key].comments = []
            }
            const viewComments = this.comments;
            for (const key in viewComments) {
                viewComments[key].user = this.users[viewComments[key].userId];
            }
            for (const key in this.comments) {
                const comment = this.comments[key];
                viewPosts[comment.postId].comments.push(comment);
            }
            return Object.values(viewPosts).sort((a, b) => b.id - a.id).slice(0, 10);
        }
    }
}
</script>

<style scoped>

</style>