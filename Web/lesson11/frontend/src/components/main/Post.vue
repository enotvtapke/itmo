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
    props: ["post"]
}
</script>

<style scoped>

</style>