<script lang="ts">
  import { LD } from "@nosnibor89/svelte-client-sdk";
  import type { Readable } from "svelte/store";

  type MoviesSmartAssistantConfig = {
    model: string;
    label: string;
  };

  type MoviesSmartAssistantProps = {
    prompt: string;

    // Submits the propmt
    onSubmit: (response: string) => void;

    // Writes the prompt
    onChange: (value: string) => void;
  };

  let { prompt, onChange, onSubmit }: MoviesSmartAssistantProps = $props();

  function handleClick(e: Event) {
    onSubmit(prompt);
  }


  const smartAssistantConfig = LD.watch("smart-assistant-config") as Readable<MoviesSmartAssistantConfig>;

  $effect(() => {
      console.log("using Smart Assistant Config:", $smartAssistantConfig);
  });


  function handleInput(e: Event) {
    const inputValue = (e.target as HTMLInputElement).value;
    onChange(inputValue);
  }
</script>

<div class="flex items-center w-full border border-gray-300 rounded p-2 mb-4">
  <input
    type="text"
    placeholder={$smartAssistantConfig?.label ?? "Ask me anything..."}
    value={prompt}
    oninput={handleInput}
    class="flex-grow outline-none placeholder:italic"
  />
  <button type="button" onclick={handleClick} aria-label="Submit">
    <svg
      class="h-5 w-5 text-gray-400 ml-2 hover:text-gray-600 transition-colors duration-200"
      xmlns="http://www.w3.org/2000/svg"
      fill="currentColor"
      viewBox="0 0 24 24"
      stroke="currentColor"
    >
      <path
        stroke-linecap="round"
        stroke-linejoin="round"
        stroke-width="2"
        d="M2.5 12.5l19-8-8 19-3-7-7-3z"
      />
    </svg>
  </button>
</div>
