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

    course.course_ids = [];

    $('[data-card-title="Section"]').each((_i, el) => { // element CSS selector to find all the different rows 
      const text = $(el).text(); 
      const id = text.match(/\((\d+)\)/) // regex pattern to match group of digits inside '()' 
      course.course_ids.push(id[1]); // the first match group is without the '()' 
      });

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