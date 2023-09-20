const { faker } = require('@faker-js/faker')
const fs = require('fs')

const generators = ['faker' ]
const fakeSets = 1
const fakeParticipants = 100000

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
    for (let countDataset = 0; countDataset < fakeSets; countDataset++) {
      const logger = fs.createWriteStream(`${generators[index]}_${countDataset}.csv`, {
        flags: 'w' // 'a' means appending (old data will be preserved)
      })

      logger.write('mturk,gender,iban,ipv4,ipv6,mac,imei\n')

      for (let countParticipant = 0; countParticipant < fakeParticipants; countParticipant++) {
    
                // GENERATE - mturk id
                logger.write(`A${makeid(12)}`)
                logger.write(',')

        logger.write(faker.name.gender())
        logger.write(',')
        logger.write(faker.finance.iban())
        logger.write(',')
        logger.write(faker.internet.ipv4())
        logger.write(',')
        logger.write(faker.internet.ipv6())
        logger.write(',')
        logger.write(faker.internet.mac())
        logger.write(',')
        logger.write(faker.phone.imei())        
        logger.write('\n')
      }
      logger.end()
    }
  }
