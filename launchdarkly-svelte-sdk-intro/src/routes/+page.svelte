<script lang="ts">
  import movies from "../movies.json";
  import MovieCard from "$lib/MovieCard.svelte";
  import SearchBar from "$lib/SearchBar.svelte";
  import type { Movie } from "$lib/model";

  let searchQuery = $state("");
  let filteredMovies = $derived.by<Movie[]>(() => {
    let filtered = movies as Movie[];
    if (searchQuery) {
      filtered = movies.filter((movie) =>
        movie.title.toLowerCase().includes(searchQuery.toLowerCase())
      );
    }
  });

  function handleSearch(searchText: string) {
    searchQuery = searchText;
  }
</script>

<div class="container mx-auto p-4">
  <SearchBar value={searchQuery} onSearch={handleSearch} />
  <div
    class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4"
  >
    {#each filteredMovies as movie}
      <MovieCard {movie} />
    {/each}
  </div>
</div>
