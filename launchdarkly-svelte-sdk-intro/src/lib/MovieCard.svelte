<script lang="ts">
  import type { Movie } from "./model";

  type MovieCardProps = {
    movie: Movie;
  }

  const { movie } = $props<MovieCardProps>();

  let isFavorite = $state(false);

  function toggleFavorite() {
    isFavorite = !isFavorite;
  }
</script>

<div class="border p-4 rounded shadow">
  <img
    src={movie.image}
    alt={movie.title}
    class="w-full h-48 object-cover mb-4"
  />
  <div class="flex flex-col p-2 text-center">
    <h2 class="text-xl font-bold mb-2">{movie.title}</h2>
    <p class="text-gray-600 mb-2">{movie.genre}</p>
    <p class="text-yellow-500 font-bold mb-2">⭐ {movie.rating}</p>
    <button
      class="bg-blue-500 text-white px-4 py-2 rounded flex items-center justify-center gap-2"
      onclick={toggleFavorite}
    >
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="16"
        height="16"
        fill={isFavorite ? "red" : "white"}
        class="bi bi-heart-fill"
        viewBox="0 0 16 16"
      >
        <path
          fill-rule="evenodd"
          d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"
        />
      </svg>
      Favorite
    </button>
  </div>
</div>
