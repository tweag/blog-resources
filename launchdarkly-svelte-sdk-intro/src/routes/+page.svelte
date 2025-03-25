<script lang="ts">
  import movies from "../movies.json";
  import MovieCard from "$lib/MovieCard.svelte";
  import SearchBar from "$lib/SearchBar.svelte";
  import type { Movie } from "$lib/model";
  import MoviesSmartAssistant from "$lib/MoviesSmartAssistant.svelte";
  import { LDFlag } from "@nosnibor89/svelte-client-sdk";

  let searchQuery = $state("");
  let prompt = $state("");
  let filteredMovies = $derived.by<Movie[]>(() => {
    let filtered = movies as Movie[];
    if (searchQuery) {
      filtered = movies.filter((movie) =>
        movie.title.toLowerCase().includes(searchQuery.toLowerCase())
      );
    }

    return filtered;
  });

  function handleSearch(searchText: string) {
    searchQuery = searchText;
  }

  function handlePromptChange(text: string) {
    prompt = text;
  }

  function handleSendPrompt(prompt: string) {
    // Handle the response from the assistant
    console.log("proccessing prompt...");
  }
</script>

<div class="container mx-auto p-4">
  <LDFlag flag="show-movie-smart-assistant">
    {#snippet on()}
      <MoviesSmartAssistant
        {prompt}
        onChange={handlePromptChange}
        onSubmit={handleSendPrompt}
      />
    {/snippet}
    {#snippet off()}
      <SearchBar value={searchQuery} onSearch={handleSearch} />
    {/snippet}
  </LDFlag>

  <div
    class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4"
  >
    {#each filteredMovies as movie}
      <MovieCard {movie} />
    {/each}
  </div>
</div>
