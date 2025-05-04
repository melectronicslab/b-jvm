<script lang="ts">
	import { makeBovineOS } from '../lib/bjvm_links/bjvm_interface/bjvm2.ts';
</script>

<div id="wrapper">
	<div id="app">
		<h1>bjvm</h1>
		<p>Bring back the applet!</p>
	</div>
</div>

<style>
	:root {
		font-family: system-ui, Avenir, Helvetica, Arial, sans-serif;
		line-height: 1.5;
		font-weight: 400;

		color-scheme: light dark;
		color: rgba(255, 255, 255, 0.87);
		background-color: #111;

		font-synthesis: none;
		text-rendering: optimizeLegibility;
		-webkit-font-smoothing: antialiased;
		-moz-osx-font-smoothing: grayscale;
	}

	a {
		font-weight: 500;
		color: #646cff;
		text-decoration: inherit;
	}

	a:hover {
		color: #535bf2;
	}

	#wrapper {
		margin: 0;
		display: flex;
		min-width: 320px;
		min-height: 100vh;
	}

	#app {
		max-width: 1280px;
		margin: 0 auto;
		padding: 2rem;
		text-align: center;
	}

	@media (prefers-color-scheme: light) {
		:root {
			color: #213547;
			background-color: #ffffff;
		}
		a:hover {
			color: #747bff;
		}
		button {
			background-color: #f9f9f9;
		}
	}
</style>
