const { faker } = require('@faker-js/faker')
const chance = require('chance').Chance()
const fs = require('fs')

const generators = ['faker', 'chance']
const fakeSets = 1
const fakeParticipants = 50

function makeid (length) {
  let result = ''
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
  const charactersLength = characters.length
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() *
charactersLength))
  }
  return result
}

for (let index = 0; index < generators.length; index++) {
  if (generators[index] === 'faker') {
    for (let countDataset = 0; countDataset < fakeSets; countDataset++) {
      const logger = fs.createWriteStream(`${generators[index]}_${countDataset}.csv`, {
        flags: 'w' // 'a' means appending (old data will be preserved)
      })

      logger.write('firstName,lastName,gender,city,cityName,cityPrefix,citySuffix,country,countryCode,county,direction,latitude,longitude,ordinalDirection,secondaryAddress,state,stateAbbr,streetAddress,streetName,streetPrefix,streetSuffix,timeZone,zipCode,creditCardNumber,iban,email,ipv4,ipv6,mac,imei,phoneNumber\n')

      for (let countParticipant = 0; countParticipant < fakeParticipants; countParticipant++) {
        logger.write(faker.name.firstName())
        logger.write(',')
        logger.write(faker.name.lastName())
        logger.write(',')
        logger.write(faker.name.gender())
        logger.write(',')
        logger.write(faker.address.city())
        logger.write(',')
        logger.write(faker.address.cityName())
        logger.write(',')
        logger.write(faker.address.cityPrefix())
        logger.write(',')
        logger.write(faker.address.citySuffix())
        logger.write(',')
        logger.write(faker.address.country())
        logger.write(',')
        logger.write(faker.address.countryCode())
        logger.write(',')
        logger.write(faker.address.county())
        logger.write(',')
        logger.write(faker.address.direction())
        logger.write(',')
        logger.write(faker.address.latitude())
        logger.write(',')
        logger.write(faker.address.longitude())
        // Deleted this because it is an array of lat + long
        // logger.write(',')
        // logger.write(faker.address.nearbyGPSCoordinate())
        logger.write(',')
        logger.write(faker.address.ordinalDirection())
        logger.write(',')
        logger.write(faker.address.secondaryAddress())
        logger.write(',')
        logger.write(faker.address.state())
        logger.write(',')
        logger.write(faker.address.stateAbbr())
        logger.write(',')
        logger.write(faker.address.streetAddress())
        logger.write(',')
        logger.write(faker.address.streetName())
        logger.write(',')
        logger.write(faker.address.streetPrefix())
        logger.write(',')
        logger.write(faker.address.streetSuffix())
        logger.write(',')
        logger.write(faker.address.timeZone())
        logger.write(',')
        logger.write(faker.address.zipCode())
        logger.write(',')
        logger.write(faker.finance.creditCardNumber())
        logger.write(',')
        logger.write(faker.finance.iban())
        logger.write(',')
        logger.write(faker.internet.email())
        logger.write(',')
        logger.write(faker.internet.ipv4())
        logger.write(',')
        logger.write(faker.internet.ipv6())
        logger.write(',')
        logger.write(faker.internet.mac())
        logger.write(',')
        logger.write(faker.phone.imei())
        logger.write(',')
        logger.write(faker.phone.phoneNumber())
        logger.write('\n')
      }
      logger.end()
    }
  } else if (generators[index] === 'chance') {
    for (let countDataset = 0; countDataset < fakeSets; countDataset++) {
      const logger = fs.createWriteStream(`${generators[index]}_${countDataset}.csv`, {
        flags: 'w'
      })

      logger.write('firstName,lastName,initials,fullName,gender,text,mturk,email,ip,ipv6,twitter,address,latitude,longitude,phone,zip\n')

      for (let countParticipant = 0; countParticipant < fakeParticipants; countParticipant++) {
        const first = chance.first()
        const last = chance.last()
        logger.write(first)
        logger.write(',')

        logger.write(last)
        logger.write(',')

        logger.write(first.substr(0, 1) + last.substr(0, 1))
        logger.write(',')

        logger.write(`${first} ${last}`)
        logger.write(',')

        logger.write(chance.gender())
        logger.write(',')

        logger.write(chance.paragraph())
        logger.write(',')

        // GENERATE - mturk id
        logger.write(`A${makeid(12)}`)
        logger.write(',')

        logger.write(chance.email())
        logger.write(',')

        logger.write(chance.ip())
        logger.write(',')

        logger.write(chance.ipv6())
        logger.write(',')

        logger.write(chance.twitter())
        logger.write(',')

        logger.write(chance.address())
        logger.write(',')

        logger.write(chance.latitude().toString())
        logger.write(',')

        logger.write(chance.longitude().toString())
        logger.write(',')

        logger.write(chance.phone())
        logger.write(',')

        logger.write(chance.zip())
        logger.write('\n')
      }
      logger.end()
    }
  }
}
