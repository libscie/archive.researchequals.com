const fs = require("fs");

// https://developer.osf.io/#tag/General-Usage
const minimumDate = "2022-01-01";
const maximumDate = "2022-06-30";
const publicBoolean = true;
// You may need to restart this if you have a lot of pages.
// Recommended to make your search as specific as possible.
const page = 0;
let searchUrl = `https://api.osf.io/v2/nodes/?filter[date_created][gt]=${minimumDate}&filter[date_created][lt]=${maximumDate}&filter[public]=${publicBoolean}`;
const outputFile = "osf-data-csv-urls.csv";

let logger;
if (page > 0) {
  searchUrl = `https://api.osf.io/v2/nodes/?filter[date_created][gt]=${minimumDate}&filter[date_created][lt]=${maximumDate}&filter[public]=${publicBoolean}&page=${page}`;
  logger = fs.createWriteStream(outputFile, {
    flags: "a", // 'a' means appending (old data will be preserved)
  });
} else {
  logger = fs.createWriteStream(outputFile, {
    flags: "w", // 'a' means appending (old data will be preserved)
  });
}
const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// Helper function
const queryJson = async (url) => {
  let json;
  try {
    let res = await fetch(url);
    json = await res.json();
  } catch (e) {
    json = [];
  }

  return json;
};

const getProjects = async (json) => {
  try {
    json.data.map(async (project) => {
      await getProject(project);
    });

    // Pagination until no more pages left
    if (json.links.next != null) {
      console.log(json.links.next);
      await traverseOSF(json.links.next);
    }
  } catch (e) {
    console.log(`Warning: ${e}`);
  }
};

const getProject = async (project) => {
  await getChildProject(project.relationships.children.links.related.href);
  await getStorageProviders(project.relationships.files.links.related.href);
};

const getChildProject = async (url) => {
  await delay(1000);

  let json = await queryJson(url);
  try {
    json.data.map(async (project) => {
      await getProject(project);
    });

    // Pagination
    if (json.links.next != null) {
      await getStorageProviders(json.links.next);
    }
  } catch (e) {
    console.log(`Warning: ${e} (${url})`);
  }
};

const getStorageProviders = async (url) => {
  await delay(1000);

  let json = await queryJson(url);

  try {
    json.data.map(async (storage) => {
      if (storage.attributes.provider === "osfstorage") {
        await getFiles(storage.relationships.files.links.related.href);
      }
    });

    // Pagination
    if (json.links.next != null) {
      await getStorageProviders(json.links.next);
    }
  } catch (e) {
    console.log(`Warning: ${e} (${url})`);
  }
};

const getFiles = async (url) => {
  await delay(1000);

  let json = await queryJson(url);
  try {
    if (json.data) {
      json.data.map(async (file) => {
        if (file.attributes.kind === "folder") {
          try {
            await getFiles(file.relationships.files.links.related.href);
          } catch (e) {
            console.log(
              `Failure: ${file.relationships.files.links.related.href}`
            );
          }
        } else {
          if (file.attributes.name.endsWith(".csv")) {
            // Print out the files with csv
            logger.write(file.links.download);
            logger.write("\n");
          }
        }
      });
    }

    // Pagination
    if (json.links) {
      if (json.links.next != null) {
        getFiles(json.links.next);
      }
    }
  } catch (e) {
    console.log(`Warning: ${e} (${url})`);
  }
};

// Bringing it all together
const traverseOSF = async (searchUrl) => {
  await delay(1000);

  let json = await queryJson(searchUrl);

  getProjects(json);
};

traverseOSF(searchUrl);
