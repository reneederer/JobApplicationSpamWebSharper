;set client_encoding to 'UTF8';

drop table if exists link cascade;
drop table if exists sentStatus cascade;
drop table if exists sentStatusValue cascade;
drop table if exists pageMap cascade;
drop table if exists htmlPage cascade;
drop table if exists filePage cascade;
drop table if exists page cascade;
drop table if exists application cascade;
drop table if exists document cascade;
drop table if exists htmlPageTemplate cascade;
drop table if exists jobRequirement cascade;
drop table if exists jobOffer cascade;
drop table if exists employer cascade;
drop table if exists users cascade;
drop table if exists userValues cascade;

create table userValues ( id serial primary key,
                          userId int not null,
                          email text not null,
					      gender text not null,
					      degree text not null,
					      firstName text not null,
					      lastName text not null,
					      street text not null,
					      postcode text not null,
					      city text not null,
					      phone text not null,
					      mobilePhone text not null);

create table users ( id serial primary key,
                     createdOn date not null,
                     password text not null,
                     salt text not null,
                     confirmEmailGuid text null,
                     sessionGuid text unique null);

create table employer ( id serial primary key,
						company text not null,
						street text not null,
						postcode text not null,
						city text not null,
						gender text not null,
						degree text not null,
						firstName text not null,
						lastName text not null,
						email text not null,
						phone text not null,
						mobilePhone text not null);
create table jobOffer ( id serial primary key,
                        url text not null,
						jobName text not null);
create table jobRequirement ( id serial primary key,
                              jobOfferId int not null,
							  key text not null,
							  value text not null,
							  foreign key(jobOfferId) references jobOffer(id));
create table htmlPageTemplate ( id serial primary key,
                                name text unique not null,
								odtPath text unique not null,
								html text not null);
create table document ( id serial primary key,
                        userValuesId int not null,
						name text not null,
						jobName text not null,
						customVariables text not null,
						emailSubject text not null,
						emailBody text not null,
						foreign key(userValuesId) references userValues(id));

create table page ( id serial primary key,
                    documentId int not null,
					pageIndex int not null,
					foreign key(documentId) references document(id));
					--//constraint page_unique unique(documentId, pageIndex));
create table filePage( id serial primary key,
                       pageId int not null,
					   path text not null,
					   name text not null,
					   foreign key(pageId) references page(id));
create table htmlPage( id serial primary key,
                       pageId int not null,
					   templateId int null,
					   name text not null,
					   foreign key(pageId) references page(id),
					   foreign key(templateId) references htmlPageTemplate(id));

create table pageMap ( id serial primary key,
                       documentId int not null,
					   pageIndex int not null,
					   key text not null,
					   value text not null,
					   foreign key(documentId) references document(id),
					   constraint pageMap_unique unique(documentId, pageIndex, key));
create table application ( id serial primary key,
                           documentId int not null,
						   userId int not null,
						   employerId int not null,
						   foreign key(documentId) references document(id),
						   foreign key(userId) references users(id),
						   foreign key(employerId) references employer(id));
create table sentStatusValue ( id int primary key,
                               status text not null);
create table sentStatus ( id serial primary key,
                          applicationId int not null,
						  statusChangedOn date not null,
						  dueOn timestamp null,
						  sentStatusValueId int not null,
						  statusMessage text not null,
						  foreign key(applicationId) references application(id),
						  foreign key(sentStatusValueId) references sentStatusValue(id));
create table link ( id serial primary key,
                    path text not null,
					name text not null,
					guid text not null);

insert into users (password, salt, confirmEmailGuid, sessionGuid, createdOn) values('r99n/4/4NGGeD7pn4I1STI2rI+BFweUmzAqkxwLUzFP9aB7g4zR5CBHx+Nz2yn3NbiY7/plf4ZRGPaXXnQvFsA==', 'JjjYQTWgutm4pv/VnzgHf6r4NjNrAVcTq+xnR7/JsRGAIHRdrcw3IMVrzngn2KPRakfX/S1kl9VrqwAT+T02Og==', null, null, current_date);
insert into userValues (userId, email, gender, degree, firstName, lastName, street, postcode, city, phone, mobilePhone) values(1, 'someMail@myMail.de', 'f', 'Prof.', 'Astrid', 'Meier', 'Blaustr. 4', '80921', 'Muenchen', 'mein Telefon', 'mein Handy');
insert into userValues (userId, email, gender, degree, firstName, lastName, street, postcode, city, phone, mobilePhone) values(1, 'rene.ederer.nbg@gmail.com', 'm', '', 'Rene', 'Ederer', 'Raabstr. 24A', '90429', 'Nuernberg', 'kein Telefon', 'kein Handy');

insert into users (password, salt, confirmEmailGuid, sessionGuid, createdOn) values('741f5ukEhGVNSUlqZLz7YAgfHGR8wpzsoT5LqtYsESz7PxdUbyUOgLQFxafgQJ6iZ7H3VumrEjcLDzJ84wtIXA==', 'aQnjb/L1KEg285kjQKkHPy/kuNcJOWZRRziBi+CjtFximm0/7AFaBbwZC4WJXXkXCcW71CRtc4G9Oulajayd4A==', '7fb7ee1bda734c95bdfc78991cd0e5d0', null, current_date);
insert into userValues (userId, email, gender, degree, firstName, lastName, street, postcode, city, phone, mobilePhone) values(2, 'helmut@goerke.de', 'm', '', 'Helmut', 'Goerke', 'Raabstr. 24A', '90429', 'Nuernberg', '0911', '0151');
insert into document(userValuesId, name, jobName, customVariables, emailSubject, emailBody) values( 1, 'welt', 'Fachinformatiker', '', 'subject1', 'body1');
insert into document( userValuesId,name, jobName, customVariables, emailSubject, emailBody) values( 1, 'hallo', 'Test', '', 'subject2', 'body2');
insert into page(documentId, pageIndex) values(1, 2);
insert into page(documentId, pageIndex) values(1, 1);
insert into filePage(path, pageId, name) values('user/1/bewerbung_neu.odt', 1, 'datei 1');
insert into filePage(path, pageId, name) values('User/1/bewerbung_neu.odt', 2, 'datei 2');
insert into page(documentId, pageIndex) values(2, 1);
insert into filePage(path, pageId, name) values('User/1/bewerbung_neu.odt', 3, 'dtei 1');

insert into sentStatusValue(id, status) values(1, 'Application queued for sending');
insert into sentStatusValue(id, status) values(2, 'Waiting for reply after sending application');
insert into sentStatusValue(id, status) values(3, 'Appointment for interview');
insert into sentStatusValue(id, status) values(4, 'Application rejected without an interview');
insert into sentStatusValue(id, status) values(5, 'Waiting for reply after interview');
insert into sentStatusValue(id, status) values(6, 'Application rejected after interview');
insert into sentStatusValue(id, status) values(7, 'Aapplication accepted after interview');


insert into employer ( company, street, postcode, city, gender, degree, firstName, lastName, email, phone, mobilePhone)
              values ('A company', '', '', '', 'm', '', '', '', '', '', '');
insert into employer ( company, street, postcode, city, gender, degree, firstName, lastName, email, phone, mobilePhone)
              values ('B company', '', '', '', 'm', '', '', '', '', '', '');
insert into application ( userId, documentId, employerId)
                 values (1, 1, 1);
insert into application ( userId, documentId, employerId)
                 values (1, 2, 2);
insert into sentStatus ( applicationId,
						  statusChangedOn,
						  dueOn,
						  sentStatusValueId,
						  statusMessage)
                values ( 1, '2007-06-20', null, 1, '');
insert into sentStatus ( applicationId,
						  statusChangedOn,
						  dueOn,
						  sentStatusValueId,
						  statusMessage)
                values ( 2, '2007-06-20', null, 1, '');

