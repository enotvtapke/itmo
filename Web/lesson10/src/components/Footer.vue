<template>
    <footer>
        <div class="counter">
            {{ usersCount }} users | {{ postsCount }} posts
        </div>
        <a href="/">Codeforces</a> &copy; 2021 by Mike Mirzayanov
    </footer>
</template>

<script>
export default {
    name: "Footer",
    props: ["usersCount", "postsCount"]
}
</script>

<style scoped>
    .counter {
        margin-top: 0.5rem;
        margin-bottom: 0.5rem;
    }
</style>