# WeatherWander

WeatherWander is a command-line application developed in Haskell that suggests the best places to visit in a city based on the current weather conditions. By integrating real-time weather data and a curated list of locations, WeatherWander provides personalized recommendations to enhance your outdoor experiences.

## Features

- **Weather-Specific Recommendations:** Get suggestions tailored to the current weather conditions, like temperature and rain probability.
- **City-Based Search:** Input any city to receive location-specific advice.
- **CLI-Based Interface:** Easy-to-use command-line interface for quick access.
- **Real-Time Data:** Utilizes up-to-date weather information from a reliable API.
- **Database Optimization:** Stores recent weather data to reduce API calls and improve performance.

## Application Flow

WeatherWander operates on an intelligent flow to provide the best user experience while optimizing resource usage:

1. **City Input:** The user inputs a city name.
2. **Database Check:** The application checks if the city's weather data is already present in the local database and whether it was updated within the last hour.
   - If the data is recent, it is used directly from the database.
   - If not, WeatherWander fetches the latest weather data from the WeatherStack API.
3. **Database Update/Insertion:** Weather data is then updated or inserted into the database.
4. **Activity Suggestions:** Based on the current weather conditions (temperature, chance of rain, etc.), the application queries the database for suitable activities and locations in the specified city.

## Getting Started

### Prerequisites

- Haskell
- Stack
- Access to a weather API (i.e, WeatherStack)

### Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/shashankyadav03/WeatherWander.git
   ```
2. Navigate to the project directory:
   ```sh
   cd haskell-project
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
If city is more than two words, use double quotes.


## Contributing

Contributions to WeatherWander are welcome! Please read `CONTRIBUTING.md` for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the QMUL License - see the `LICENSE` file for details.

## Acknowledgments

- Weather API used: [WeatherStack](https://weatherstack.com/documentation)
- Haskell Community for support and resources
