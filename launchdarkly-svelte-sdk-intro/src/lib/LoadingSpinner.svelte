<script lang="ts">
  interface LoadingSpinnerProps {
    size?: string;
    color?: string;
    message?: string;
  }

  const props: LoadingSpinnerProps = $props();
  const size: string = props.size || "8";
  const color: string = props.color || "text-gray-500";
  const message: string = props.message || "";
</script>

<div class={`flex flex-col justify-center items-center ${size}`}>
  <svg
    class={`animate-spin h-${size} w-${size} ${color}`}
    xmlns="http://www.w3.org/2000/svg"
    fill="none"
    viewBox="0 0 24 24"
  >
    <circle
      class="opacity-25"
      cx="12"
      cy="12"
      r="10"
      stroke="currentColor"
      stroke-width="4"
    ></circle>
    <path
      class="opacity-75"
      fill="currentColor"
      d="M4 12a8 8 0 018-8v4a4 4 0 00-4 4H4z"
    ></path>
  </svg>
  {#if message}
    <p class="mt-2 text-center text-lg font-semibold text-gray-700">{message}</p>
  {/if}
</div>
