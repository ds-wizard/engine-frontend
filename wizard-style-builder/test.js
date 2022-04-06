const server = require('./index.js')
const supertest = require('supertest')
const requestWithSupertest = supertest(server)

describe('Wizard Style Server', () => {
    it('POST /simple should generate style', async () => {
        const res = await requestWithSupertest.post('/simple')

        expect(res.status).toEqual(200)
        expect(res.headers['content-type']).toMatch(/text\/css/)
    })

    it('POST /simple with logo URL', async () => {
        const logoUrl = 'https://example.com/logo.png'
        const res = await requestWithSupertest.post('/simple').send({logoUrl})

        expect(res.status).toEqual(200)
        expect(res.text).toContain(logoUrl)
    })

    afterAll(async () => {
        await server.close()
    })
})