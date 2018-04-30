;set client_encoding to 'UTF8';

drop table if exists link cascade;
drop table if exists sentStatus cascade;
drop table if exists sentStatusValue cascade;
drop table if exists pageMap cascade;
drop table if exists documentEditedOn cascade;
drop table if exists htmlPage cascade;
drop table if exists filePage cascade;
drop table if exists page cascade;
drop table if exists document cascade;
drop table if exists htmlPageTemplate cascade;
drop table if exists jobRequirement cascade;
drop table if exists jobOffer cascade;
drop table if exists employer cascade;
drop table if exists login cascade;
drop table if exists users cascade;


create table users ( id serial primary key,
                     email text unique null,
					 password text not null,
					 salt text not null,
					 confirmEmailGuid text null,
					 sessionGuid text unique null,
					 createdOn date not null,
					 deletedOn date null,
					 gender text not null,
					 degree text not null,
					 name text not null,
					 street text not null,
					 postcode text not null,
					 city text not null,
					 phone text not null,
					 mobilePhone text not null);
create table login ( id serial primary key,
                     userId int not null,
					 loggedInAt timestamp with time zone not null,
					 loggedInForSeconds int not null,
					 foreign key(userId) references users(id));
create table employer ( id serial primary key,
                        userId int not null,
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
						mobilePhone text not null,
						foreign key(userId) references users(id));
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
                        userId int not null,
						name text not null,
						jobName text not null,
						customVariables text not null,
						emailSubject text not null,
						emailBody text not null,
						createdOn date not null,
						deletedOn date null,
						foreign key(userId) references users(id));

create table page ( id serial primary key,
                    documentId int not null,
					pageIndex int not null,
					foreign key(documentId) references document(id),
					constraint page_unique unique(documentId, pageIndex));
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

create table documentEditedOn ( id serial primary key,
								documentId int not null,
								editedOn date not null,
								foreign key(documentId) references document(id));
create table pageMap ( id serial primary key,
                       documentId int not null,
					   pageIndex int not null,
					   key text not null,
					   value text not null,
					   foreign key(documentId) references document(id),
					   constraint pageMap_unique unique(documentId, pageIndex, key));
create table sentStatusValue ( id int primary key,
                               status text not null);
create table sentStatus ( id serial primary key,
                          documentId int not null,
						  statusChangedOn date not null,
						  dueOn timestamp null,
						  sentStatusValueId int not null,
						  statusMessage text not null,
						  foreign key(sentStatusValueId) references sentStatusValue(id));
create table link ( id serial primary key,
                    path text not null,
					guid text not null,
					name text not null);

insert into users ( email, password, salt, confirmEmailGuid, sessionGuid, createdOn, gender, degree, name, street, postcode, city, phone, mobilePhone) values('rene.ederer.nbg@gmail.com', 'r99n/4/4NGGeD7pn4I1STI2rI+BFweUmzAqkxwLUzFP9aB7g4zR5CBHx+Nz2yn3NbiY7/plf4ZRGPaXXnQvFsA==', 'JjjYQTWgutm4pv/VnzgHf6r4NjNrAVcTq+xnR7/JsRGAIHRdrcw3IMVrzngn2KPRakfX/S1kl9VrqwAT+T02Og==', null, null, current_date, 'm', '', 'René Ederer', 'Raabstr. 24A', '90429', 'Nürnberg', 'kein Telefon', 'kein Handy');
insert into document(userId, name, createdOn, jobName, customVariables, emailSubject, emailBody) values(1, 'welt', '1982-07-19 13:52:38', 'Fachinformatiker', '', 'subject1', 'body1');
insert into document(userId, name, createdOn, jobName, customVariables, emailSubject, emailBody) values(1, 'hallo', '1982-07-19 13:52:38', 'Test', '', 'subject2', 'body2');
insert into page(id, documentId, pageIndex) values(1, 1, 2);
insert into page(id, documentId, pageIndex) values(2, 1, 1);
insert into filePage(path, pageId, name) values('Users/1/bewerbung_neu.odt', 1, 'datei 1');
insert into filePage(path, pageId, name) values('Users/1/bewerbung_neu.odt', 1, 'datei 2');
insert into page(id, documentId, pageIndex) values(3, 2, 1);
insert into filePage(path, pageId, name) values('Users/1/bewerbung_neu.odt', 3, 'dtei 1');

/*
insert into document(userId, name, jobName, customVariables) values(1, 'mein zweites htmlTemplate', 'Automechaniker', '');
insert into documentEmail(documentId, subject, body) values(1, 'Bewerbung als $beruf', 'Sehr $geehrter $chefAnrede $chefTitel $chefNachname,\n\nanbei sende ich Ihnen meine Bewerbungsunterlagen.\nÜber eine Einladung zu einem Bewerbungsgespräch freue ich mich sehr.\n\nMit freundlichen Grüßen\n\n\n$meinTitel $meinVorname $meinNachname\n$meineStrasse\n$meinePlz $meineStadt\n$meineMobilnummer');
insert into filePage(documentId, path, pageIndex, name) values(1, 'Users/1/labenwolf_zeugnis_small.pdf', 2, 'Labenwolf Zeugnis');
insert into filePage(documentId, path, pageIndex, name) values(1, 'Users/1/bewerbung_neu1.odt', 3, 'Anschreiben');
insert into filePage(documentId, path, pageIndex, name) values(1, 'Users/1/segitz_zeugnis_small.pdf', 4, 'Labenwolf Zeugnis');
*/

insert into sentStatusValue(id, status) values(1, 'Application queued for sending');
insert into sentStatusValue(id, status) values(2, 'Waiting for reply after sending application');
insert into sentStatusValue(id, status) values(3, 'Appointment for interview');
insert into sentStatusValue(id, status) values(4, 'Application rejected without an interview');
insert into sentStatusValue(id, status) values(5, 'Waiting for reply after interview');
insert into sentStatusValue(id, status) values(6, 'Application rejected after interview');
insert into sentStatusValue(id, status) values(7, 'Aapplication accepted after interview');

