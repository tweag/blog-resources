import { Effect, Array, Option, pipe, Stream, Chunk, StreamEmit } from "effect";
import { Schema } from "@effect/schema";
import {
  FetchHttpClient,
  HttpClient,
  HttpClientResponse,
} from "@effect/platform";

const CityResponse = Schema.Struct({
  name: Schema.String,
  country_code: Schema.String.pipe(Schema.length(2)),
  latitude: Schema.Number,
  longitude: Schema.Number,
});

type CityResult = Schema.Schema.Type<typeof CityResponse>;

const GeocodingResponse = Schema.Struct({
  results: Schema.Array(CityResponse),
});

type GeocodingResponse = Schema.Schema.Type<typeof GeocodingResponse>;

const Weather = Schema.Struct({
  current_units: Schema.Struct({
    temperature_2m: Schema.String,
    relative_humidity_2m: Schema.String,
    apparent_temperature: Schema.String,
    precipitation: Schema.String,
  }),
  current: Schema.Struct({
    temperature_2m: Schema.Number,
    relative_humidity_2m: Schema.Number,
    apparent_temperature: Schema.Number,
    precipitation: Schema.Number,
  }),
});

type Weather = Schema.Schema.Type<typeof Weather>;

// The field input
const city = Option.fromNullable(
  document.querySelector<HTMLInputElement>("#city"),
);
// The list of suggestions
const cities = Option.fromNullable(
  document.querySelector<HTMLUListElement>("#cities"),
);
// The weather information
const weather = Option.fromNullable(
  document.querySelector<HTMLDivElement>("#weather"),
);

const getRequest = (url: string) =>
  // Effect.gen(function* () {
  //   const client = yield* HttpClient.HttpClient;
  //
  //   return yield* pipe(
  //     client.get(url),
  //     HttpClient.withTracerPropagation(false),
  //   );
  // }).pipe(Effect.provide(FetchHttpClient.layer));
  pipe(
    HttpClient.HttpClient,
    Effect.andThen((client) => client.get(url)),
    HttpClient.withTracerPropagation(false),
    Effect.provide(FetchHttpClient.layer),
  );

const getCities = function (search: string) {
  Option.map(cities, (c) => (c.innerHTML = ""));

  return pipe(
    getCity(search),
    Effect.map(renderCity),
    // Check if the input is empty
    Effect.when(() => Boolean(search)),
  );
};

Option.map(city, (cityEl) => {
  const stream = Stream.async(
    (emit: StreamEmit.Emit<never, never, string, void>) =>
      cityEl.addEventListener("input", function (_event) {
        emit(Effect.succeed(Chunk.of(this.value)));
      }),
  );

  pipe(
    stream,
    Stream.debounce(500),
    Stream.runForEach(getCities),
    Effect.runPromise,
  );
});

const getCity = (city: string) =>
  // Effect.gen(function* () {
  //   const response = yield* getRequest(
  //     `https://geocoding-api.open-meteo.com/v1/search?name=${city}&count=10&language=en&format=json`,
  //   );
  //
  //   const geocoding = yield* pipe(
  //     response,
  //     HttpClientResponse.schemaBodyJson(GeocodingResponse), // Validate the response against the schema, it adds the `ParseResult.ParseError` to the `Error` type of the effect
  //     Effect.orElseSucceed<GeocodingResponse>(() => ({ results: [] })), // If the effect is in a failure state, we can provide a default value to turn the effect into a success state
  //   );
  //
  //   return geocoding.results;
  // }).pipe(Effect.scoped);
  pipe(
    getRequest(
      `https://geocoding-api.open-meteo.com/v1/search?name=${city}&count=10&language=en&format=json`,
    ),
    Effect.andThen(HttpClientResponse.schemaBodyJson(GeocodingResponse)),
    Effect.orElseSucceed<GeocodingResponse>(() => ({ results: [] })),
    Effect.map((geocoding) => geocoding.results),
    Effect.scoped,
  );

const renderCity = (cities: readonly CityResult[]) => {
  // If there are multiple cities, populate the suggestions
  if (cities.length > 1) {
    populateSuggestions(cities);
    return;
  }

  // We didn't get into the if statement above, so we have only one city or none
  // Let's try to get the first city
  pipe(
    Array.head(cities),
    Option.match({
      onSome: selectCity,
      onNone: () => {
        const search = Option.match(city, {
          onSome: (cityEl) => cityEl.value,
          onNone: () => "searched",
        });

        Option.map(
          weather,
          (weatherEl) =>
            (weatherEl.innerHTML = `<p>City ${search} not found</p>`),
        );
      },
    }),
  );
};

const populateSuggestions = (results: readonly CityResult[]) =>
  Option.map(cities, (citiesEl) =>
    results.forEach((city) => {
      const li = document.createElement("li");
      li.innerText = `${city.name} - ${city.country_code}`;
      li.addEventListener("click", () => selectCity(city));
      citiesEl.appendChild(li);
    }),
  );

const selectCity = (result: CityResult) =>
  Option.map(weather, (weatherEl) =>
    pipe(
      result,
      getWeather,
      Effect.match({
        onFailure: (error) =>
          (weatherEl.innerHTML = `<p>An error occurred while fetching the weather: ${error}</p>`),
        onSuccess: (weatherData: Weather) =>
          (weatherEl.innerHTML = `
<h2>${result.name}</h2>
<p>Temperature: ${weatherData.current.temperature_2m}°C</p>
<p>Feels like: ${weatherData.current.apparent_temperature}°C</p>
<p>Humidity: ${weatherData.current.relative_humidity_2m}%</p>
<p>Precipitation: ${weatherData.current.precipitation}mm</p>
`),
      }),
      Effect.runPromise,
    ),
  );

const getWeather = (result: CityResult) =>
  pipe(
    getRequest(
      `https://api.open-meteo.com/v1/forecast?latitude=${result.latitude}&longitude=${result.longitude}&current=temperature_2m,relative_humidity_2m,apparent_temperature,precipitation&timezone=auto&forecast_days=1`,
    ),
    Effect.andThen(HttpClientResponse.schemaBodyJson(Weather)),
    Effect.scoped,
  );
// Effect.gen(function* () {
//   const response = yield* getRequest(
//     `https://api.open-meteo.com/v1/forecast?latitude=${result.latitude}&longitude=${result.longitude}&current=temperature_2m,relative_humidity_2m,apparent_temperature,precipitation&timezone=auto&forecast_days=1`,
//   );
//
//   return yield* HttpClientResponse.schemaBodyJson(Weather)(response);
// }).pipe(Effect.scoped, Effect.provide(FetchHttpClient.layer));
