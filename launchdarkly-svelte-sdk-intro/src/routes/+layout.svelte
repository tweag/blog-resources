<script lang="ts">
  import "../app.css";
//   import { LDProvider } from "@nosnibor89/svelte-client-sdk";

  let { children } = $props();

//   const ldConfig = {
//     clientID: "YOUR_CLIENT_SIDE_ID", // Replace with your actual client-side ID
//   };
//   const ldContext = {
//     key: "guest-user",
//     anonymous: true,
//   };
</script>

<!-- <LDProvider clientID={ldConfig.clientID} context={ldContext}> -->
  {@render children()}
<!-- </LDProvider> -->
