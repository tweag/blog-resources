<script lang="ts">
  import "../app.css";
  import { LDProvider } from "@nosnibor89/svelte-client-sdk";
  import { PUBLIC_LD_CLIENT_ID } from '$env/static/public';
  import LoadingSpinner from "$lib/LoadingSpinner.svelte";

  let { children } = $props();

  // random beetwen 0 and 1
  const orgId = Math.round(Math.random());

  const orgKey = `sdk-example-org-${orgId}`


  const ldContext = {
    kind: "org",
    key: orgKey,
  };

  console.log("LD Context:", ldContext);

</script>

<LDProvider clientID={PUBLIC_LD_CLIENT_ID} context={ldContext}>
  {#snippet initializing()}
    <div class="flex items-center justify-center h-screen">
      <LoadingSpinner message={"Loading flags"}/>
    </div>
  {/snippet}

  {@render children()}
</LDProvider>
