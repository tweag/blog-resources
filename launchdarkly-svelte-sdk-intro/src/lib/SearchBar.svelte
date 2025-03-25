<script lang="ts">
  type SearchBarProps = {
    value: string;
    onSearch: (value: string) => void;
  };

  let { value, onSearch }: SearchBarProps = $props();

  function handleInput(e: Event) {
    const inputValue = (e.target as HTMLInputElement).value;
    onSearch(inputValue);
  }
</script>

<div class="flex items-center w-full border border-gray-300 rounded p-2 mb-4">
  <input
    type="text"
    placeholder="Search movies..."
    {value}
    oninput={handleInput}
    class="flex-grow outline-none placeholder:italic"
  />
  <svg
    class="h-5 w-5 text-gray-400 ml-2"
    xmlns="http://www.w3.org/2000/svg"
    fill="none"
    viewBox="0 0 24 24"
    stroke="currentColor"
  >
    <path
      stroke-linecap="round"
      stroke-linejoin="round"
      stroke-width="2"
      d="M21 21l-4.35-4.35m1.1-5.65a7.5 7.5 0 11-15 0 7.5 7.5 0 0115 0z"
    />
  </svg>
</div>
