<script lang="ts">
  type SearchBarProps = {
    value: string;
    onSearch: (value: string) => void;
  };

  let { value, onSearch } = $props<SearchBarProps>();

  function handleInput(e: Event) {
    const inputValue = (e.target as HTMLInputElement).value;
    onSearch(inputValue);
  }
</script>

<input
  type="text"
  placeholder="Search movies..."
  {value}
  oninput={handleInput}
  class="w-full p-2 mb-4 border border-gray-300 rounded"
/>
