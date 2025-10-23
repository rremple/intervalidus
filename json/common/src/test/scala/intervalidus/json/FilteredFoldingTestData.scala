package intervalidus.json

trait FilteredFoldingTestData:

  val testDoc: String =
    """{
      |  "provider": {
      |    "name": {
      |      "last": "Laboratory Corp Of America Holdings"
      |    },
      |    "uniqueId": "93a6e170-0493-5211-a6f9-6bc0d9ccf87f",
      |    "education": [],
      |    "languages": [
      |      "English"
      |    ],
      |    "nationalProviderId": "9999999417",
      |    "locations": [
      |      {
      |        "specialties": [
      |          {
      |            "gibberish": 129,
      |            "rawSpecialtyCode": "200015",
      |            "moreGibberish": [
      |              "967",
      |              "810110",
      |              "710143",
      |              "610085"
      |            ],
      |            "referralTypesByGibberish": []
      |          }
      |        ],
      |        "preferredGibberish": true,
      |        "services": [],
      |        "ePrescriptionNetworks": [],
      |        "offeredProgramIds": [],
      |        "moreGibberish": [
      |          "967",
      |          "810110",
      |          "710143",
      |          "610085"
      |        ],
      |        "groupAffiliations": [],
      |        "languagesStaff": [],
      |        "officeHours": {
      |          "times": [
      |            {
      |              "day": "thursday",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "wednesday",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "saturday",
      |              "hours": [
      |                {
      |                  "start": "07:00:00",
      |                  "end": "11:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "monday",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "tuesday",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "friday",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            }
      |          ]
      |        },
      |        "freestandingFacility": true,
      |        "locationAccreditationIds": [],
      |        "customPayerDistinctions": [],
      |        "junk": [],
      |        "ageRestrictedPlans": [],
      |        "websites": [],
      |        "otherJunk": [],
      |        "religiousAffiliations": [],
      |        "religiousPrograms": {}
      |      },
      |      {
      |        "specialties": [
      |          {
      |            "gibberish": 129,
      |            "rawSpecialtyCode": "200025",
      |            "moreGibberish": [
      |              "967",
      |              "810110",
      |              "710143",
      |              "610085"
      |            ],
      |            "referralTypesByGibberish": []
      |          }
      |        ],
      |        "preferredGibberish": true,
      |        "services": [],
      |        "ePrescriptionNetworks": [],
      |        "offeredProgramIds": [],
      |        "moreGibberish": [
      |          "967",
      |          "810110",
      |          "710143",
      |          "610085"
      |        ],
      |        "groupAffiliations": [],
      |        "languagesStaff": [],
      |        "officeHours": {
      |          "times": [
      |            {
      |              "day": "thursday-alt",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "wednesday-alt",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "saturday-alt",
      |              "hours": [
      |                {
      |                  "start": "07:00:00",
      |                  "end": "11:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "monday-alt",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "tuesday-alt",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            },
      |            {
      |              "day": "friday-alt",
      |              "hours": [
      |                {
      |                  "start": "06:30:00",
      |                  "end": "15:00:00"
      |                }
      |              ]
      |            }
      |          ]
      |        },
      |        "freestandingFacility": true,
      |        "locationAccreditationIds": [],
      |        "customPayerDistinctions": [],
      |        "junk": [],
      |        "ageRestrictedPlans": [],
      |        "websites": [],
      |        "otherJunk": [],
      |        "religiousAffiliations": [],
      |        "religiousPrograms": {}
      |      }
      |    ]
      |  }
      |}
      |""".stripMargin
