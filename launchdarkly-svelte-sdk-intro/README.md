# LaunchDarkly Svelte SDK Introduction Example

This is a simple example project demonstrating how to use the LaunchDarkly Svelte SDK in a Svelte application.

## Installation and Running the Project

### Prerequisites

- Ensure you have Node.js installed on your system. If you use NVM, you can run `nvm use` to set the correct Node version. However, any version greater than 20 should work fine.
- A LaunchDarkly account with a client-side ID for your project.

### Steps to Install and Run

1. From root, navigate to the project directory:

   ```bash
   cd launchdarkly-svelte-sdk-intro
   ```

2. Create a `.env` file based on the example:

   ```bash
   cp .env.example .env
   ```

3. Update the `.env` file with your LaunchDarkly client-side ID:

   ```bash
   PUBLIC_LD_CLIENT_ID=your-actual-launchdarkly-client-side-id
   ```

   > **Note:** You can find your client-side ID in your LaunchDarkly project settings under "Environments". Make sure to use the client-side ID, not the SDK key.

4. Install dependencies:

   ```bash
   npm install
   ```

5. Start the development server:

   ```bash
   npm run dev
   ```

6. Open your browser and navigate to the URL provided in the terminal (usually `http://localhost:5173` but port may vary).
