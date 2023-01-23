<template>
    <div class="posts">
        <RawPost v-for="post in posts" :post="post" :key="post.id"/>
    </div>
</template>

<script>
import RawPost from "./RawPost";

export default {
    name: "Index",
    components: {
        RawPost
    },
    props: ["posts"]
}
</script>

<style scoped>

</style>