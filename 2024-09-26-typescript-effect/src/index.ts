import { Effect, Array, Option, pipe, Stream, Chunk, StreamEmit, Scope } from "effect";
import { Schema, ParseResult } from "@effect/schema";
import {
  FetchHttpClient,
  HttpClient,
  HttpClientResponse,
  HttpClientError
} from "@effect/platform";


const CityResponse = Schema.Struct({
  name: Schema.String,
  country_code: pipe(Schema.String, Schema.length(2)),
  latitude: Schema.Number,
  longitude: Schema.Number,
});

type CityResponse = Schema.Schema.Type<typeof CityResponse>;

const GeocodingResponse = Schema.Struct({
  results: Schema.Array(CityResponse),
});

type GeocodingResponse = Schema.Schema.Type<typeof GeocodingResponse>;

const WeatherResponse = Schema.Struct({
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

type WeatherResponse = Schema.Schema.Type<typeof WeatherResponse>;

// The field input
const cityElement = Option.fromNullable(
  document.querySelector<HTMLInputElement>("#city"),
);
// The list of suggestions
const citiesElement = Option.fromNullable(
  document.querySelector<HTMLUListElement>("#cities"),
);
// The weather information
const weatherElement = Option.fromNullable(
  document.querySelector<HTMLDivElement>("#weather"),
);

const getRequest = (url: string): Effect.Effect<HttpClientResponse.HttpClientResponse, HttpClientError.HttpClientError, Scope.Scope> =>
  pipe(
    HttpClient.HttpClient,
    Effect.andThen((client) => client.get(url)),
    HttpClient.withTracerPropagation(false),
    Effect.provide(FetchHttpClient.layer),
  );

const getCities = (search: string): Effect.Effect<Option.Option<void>, never, never> => {
  Option.map(citiesElement, (c) => (c.innerHTML = ""));

  return pipe(
    getCity(search),
    Effect.map(renderCitySuggestions),
    // Check if the input is empty
    Effect.when(() => Boolean(search)),
  );
};

Option.map(cityElement, (cityEl) => {
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

const getCity = (city: string): Effect.Effect<readonly CityResponse[], never, never> =>
  pipe(
    getRequest(
      `https://geocoding-api.open-meteo.com/v1/search?name=${city}&count=10&language=en&format=json`,
    ),
    Effect.andThen(HttpClientResponse.schemaBodyJson(GeocodingResponse)),
    Effect.orElseSucceed<GeocodingResponse>(() => ({ results: [] })),
    Effect.map((geocoding) => geocoding.results),
    Effect.scoped,
  );

const renderCitySuggestions = (cities: readonly CityResponse[]): void => {
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
        const search = Option.match(cityElement, {
          onSome: (cityEl) => cityEl.value,
          onNone: () => "searched",
        });

        Option.map(
          weatherElement,
          (weatherEl) =>
            (weatherEl.innerHTML = `<p>City ${search} not found</p>`),
        );
      },
    }),
  );
};

const populateSuggestions = (results: readonly CityResponse[]): Option.Option<void> =>
  Option.map(citiesElement, (citiesEl) =>
    results.forEach((city) => {
      const li = document.createElement("li");
      li.innerText = `${city.name} - ${city.country_code}`;
      li.addEventListener("click", () => selectCity(city));
      citiesEl.appendChild(li);
    }),
  );

const selectCity = (result: CityResponse): Option.Option<Promise<string>> =>
  Option.map(weatherElement, (weatherEl) =>
    pipe(
      result,
      getWeather,
      Effect.match({
        onFailure: (error) =>
          (weatherEl.innerHTML = `<p>An error occurred while fetching the weather: ${error}</p>`),
        onSuccess: (weatherData: WeatherResponse) =>
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

const getWeather = (
  result: CityResponse,
): Effect.Effect<WeatherResponse, HttpClientError.HttpClientError | ParseResult.ParseError, never> =>
  pipe(
    getRequest(
      `https://api.open-meteo.com/v1/forecast?latitude=${result.latitude}&longitude=${result.longitude}&current=temperature_2m,relative_humidity_2m,apparent_temperature,precipitation&timezone=auto&forecast_days=1`,
    ),
    Effect.andThen(HttpClientResponse.schemaBodyJson(WeatherResponse)),
    Effect.scoped,
  );
