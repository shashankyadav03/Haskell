# WeatherWander

WeatherWander is a command-line application developed in Haskell that suggests the best places to visit in a city based on the current weather conditions. By integrating real-time weather data and a curated list of locations, WeatherWander provides personalized recommendations to enhance your outdoor experiences.

## Features

- **Weather-Specific Recommendations:** Get suggestions tailored to the current weather conditions, like temperature and rain probability.
- **City-Based Search:** Input any city to receive location-specific advice.
- **CLI-Based Interface:** Easy-to-use command-line interface for quick access.
- **Real-Time Data:** Utilizes up-to-date weather information from a reliable API.

## Getting Started

### Prerequisites

- Haskell
- Stack
- Access to a weather API (e.g., OpenWeatherMap)

### Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/shashankyadav03/WeatherWander.git
   ```
2. Navigate to the project directory:
   ```sh
   cd WeatherWander
   ```
3. Build the project:
   ```sh
   stack build
   ```

### Usage

Run WeatherWander with the following command:
```sh
stack run show <city>
```
Replace `<city>` with the name of the city you want recommendations for.

## Contributing

Contributions to WeatherWander are welcome! Please read `CONTRIBUTING.md` for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the QMUL License - see the `LICENSE` file for details.

## Acknowledgments

- Weather API used: [OpenWeatherMap](https://openweathermap.org/)
- Haskell Community for support and resources
