import * as cheerio from 'cheerio';
import fs from 'fs';

async function scrape_courses(url) {

    const res = await fetch(url);
    const html = await res.text();

    const $ = cheerio.load(html)

    const courses = [];

    $('.d-flex.gap-2 h2 a').each((_i, el) => { // element CSS selector to find all the different rows
      const name = ($(el).text());
      const code = name.match(/^\S+/);
      const number = name.match(/\S\d+\S/);
      courses.push({
        'course_name': name,
        'course_code': code[0],
        'course_number': number[0]
      });
    });

/*

    const course_numbers = courses.map(course => {
      const match = course.match(/\S\d+\S/); // matches regex patterns with white space before and after (course codes)
      return match [0];
    });
        
    console.log('Course Names');
    console.log(courses);
    console.log('Course Numbers');
    console.log(course_numbers);

    */

    // console.log(courses)

    return courses

}

async function scrape_ids(url, courses) {

  for (const course of courses) {
    const combined_url = `${url}/${course.course_number}`; //add the course_number to the end of the base url

    const res = await fetch(combined_url);
    const html = await res.text();

    const $ = cheerio.load(html);

    course.course_section = [];
    course.course_ids = [];
    course.course_dates = [];
    course.course_times = [];

    $('[data-card-title="Section"]').each((_i, el) => {
      const text = $(el).text().trim();

      const sectionMatch = text.match(/(([A-Z]\d{2,3})|\d+)/);
      const idMatch = text.match(/\((\d+)\)/);
      course.course_section.push(sectionMatch[1]);
      course.course_ids.push(idMatch[1]);
    });

    $('.row.row-cols-1.row-cols-lg-3').each((_i, row) => {
      const cols = $(row).children('.col');

      course.course_dates.push($(cols[0]).text().trim());
      course.course_times.push($(cols[1]).text().trim());
    });

    console.log(course);


    //console.log("Course ID's")
    //console.log(course_ids)

  }

  return courses
}

async function run() {

  const url = "https://apps.ualberta.ca/catalogue/course/fin";

  const courses = await scrape_courses(url);
  //console.log(courses)

  const courses_with_ids = await scrape_ids(url, courses);
  //console.log(courses_with_ids)

  fs.writeFileSync(
    "courses.json",
    JSON.stringify(courses_with_ids, null, 2)
  );
}

run();