from sqlalchemy import create_engine, MetaData, Table, or_
import json
import csv
from datetime import datetime
import pickle, gzip
from numpy import mean

exp_fld = '/Users/yanivabir/Google Drive/Lab/GenderDim'
usePickle = 0
min_date = datetime(2018,7,10, 19, 0) #datetime(2018,6,24, 19, 0)

# Data base config
db_url = "mysql://greenlab:11cookies11@brms.c1lkfpz6aowj.us-east-2.rds.amazonaws.com:3306/brmsdb"
table_name = 'GenderDim'
data_column_name = 'datastring'
# boilerplace sqlalchemy setup
engine = create_engine(db_url)
metadata = MetaData()
metadata.bind = engine
table = Table(table_name, metadata, autoload=True)
s = table.select()

if not usePickle:
    q = s.where(~table.c.uniqueid.contains('debug')).where(table.c.beginhit > min_date)
    rows = q.execute()

    data = []
    # if you have workers you wish to exclude, add them here
    exclude = []
    for row in rows:
        data.append(row[data_column_name])
        print row['uniqueid']
        print row['beginhit']
        print row['cond']

    data = data
    f = gzip.open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'data_from_server','wb')
    pickle.dump(data, f)
    f.close()

else:
    f = gzip.open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'data_from_server', 'rb')
    data = pickle.load(f)
    f.close()

    ids_to_skip = {json.loads(part)['workerId'] + ":" + json.loads(part)['assignmentId'] for part in data if part}
    q = s.where(~table.c.uniqueid.contains('debug')).where((table.c.uniqueid.notin_(ids_to_skip)))
    rows = q.execute()

    for row in rows:
        data.append(row[data_column_name])
        print row['uniqueid']
        print row['beginhit']
        print row['cond']

    f = gzip.open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'data_from_server', 'wb')
    pickle.dump(data, f)
    f.close()

# parse each participant's datastring as json object
# and take the 'data' sub-object
eventdata = [{'uniqueid': json.loads(part)['workerId'] + ":" + json.loads(part)['assignmentId'],
              'data': json.loads(part)['eventdata']} for part in data if part]
jseventdata = [{'uniqueid': json.loads(part)['workerId'] + ":" + json.loads(part)['assignmentId'],
                'data': json.loads(part)['questiondata']['jsPsych_event_data']} for part in data if part and 'jsPsych_event_data' in json.loads(part)['questiondata']]
data = [{'uniqueid': json.loads(part)['workerId'] + ":" + json.loads(part)['assignmentId'],
         'data':json.loads(json.loads(part)['questiondata']['jsPsych_trial_data'])}
        for part in data if part and 'jsPsych_trial_data' in json.loads(part)['questiondata']]

# flatten brms trials and save vbl separately
print "Worker IDs for approval:"
trialTypes = ['bRMS']
brmsFieldnames = set()
brms = []
complete_subject = []
reject_subject = []
animation = []
for part in data:
    acc = []
    counter = 1
    animation.append([])
    for i in range(1, len(part['data'])):
        if part['data'][i]['internal_node_id'] == '0.0-27.0' and acc:
            complete_subject.append({'uniqueid': part['uniqueid'], 'acc': round(mean(acc), 2)})
        if part['data'][i]['internal_node_id'] == '0.0-6.2-2.2-0.2' or \
                part['data'][i]['internal_node_id'] == '0.0-1.0-0.0':
            print(part['data'][i]['stimulus'])
            reject_subject.append({'uniqueid': part['uniqueid'], 'reason': part['data'][i]['internal_node_id']})
        if part['data'][i]['trial_type'] in trialTypes:
            part['data'][i]['uniqueid'] = part['uniqueid']
            part['data'][i]['Subject'] = data.index(part) + 1
            part['data'][i]['Trial'] = counter
            if 'animation_performance' in part['data'][i]:
                animation[data.index(part)].append({'Subject': data.index(part) + 1,
                                            'uniqueid': part['uniqueid'],
                                            'Trial': counter,
                                            'animation':part['data'][i].pop('animation_performance'),
                                            'global_trial': i+1})
            brms.append(part['data'][i])
            brmsFieldnames.update(set(part['data'][i].keys()))
            if 'acc' in part['data'][i]:
                acc.append(part['data'][i]['acc'])
            counter += 1


# Flatten vbl
animationf = []
for subj in animation:
    for trial in subj:
        for flip in range(len(trial['animation']['nums'])):
            animationf.append({'Subject': trial['Subject'], 'uniqueid': trial['uniqueid'], 'Trial': trial['Trial'],
                               'nums': trial['animation']['nums'][flip], 'mond_duration': trial['animation']['mond_duration'][flip],
                               'stim_duration': trial['animation']['stim_duration'][flip]})

# Save questionnaire data
quest = []
questFieldnames = set()
for part in data:
    counter = 1
    for record in part['data']:
        if record['trial_type'] not in trialTypes:
            record['uniqueid'] = part['uniqueid']
            record['Subject'] = data.index(part) + 1
            record['Trial'] = counter
            quest.append(record)
            questFieldnames.update(set(record.keys()))
            counter += 1

for subj in (sorted(complete_subject, key=lambda k: k['uniqueid'])):
    print subj


# Flatten eventdata
eventFieldnames = set()
event = []
for part in eventdata:
    counter = 1
    for trial in part['data']:
        trial['uniqueid'] = part['uniqueid']
        trial['eventnum'] = counter
        event.append(trial)
        eventFieldnames.update(set(trial.keys()))
        counter = counter + 1

jseventFieldnames = set()
jsevent = []
for part in jseventdata:
    counter = 1
    if part['data']:
        for trial in json.loads(part['data']):
            trial['uniqueid'] = part['uniqueid']
            trial['eventnum'] = counter
            jsevent.append(trial)
            jseventFieldnames.update(set(trial.keys()))
            counter = counter + 1

# Save brms data to csv
with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'brms.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=brmsFieldnames)
    writer.writeheader()

    for record in brms:
        writer.writerow(record)

# Save quest data to csv
with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'questionnaire.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=questFieldnames)
    writer.writeheader()

    for record in quest:
        writer.writerow(record)

# Save event data to csv
with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'eventdata.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=eventFieldnames)
    writer.writeheader()

    for record in event:
        writer.writerow(record)

with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'jseventdata.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=jseventFieldnames)
    writer.writeheader()

    for record in jsevent:
        writer.writerow(record)

# Save vbl data to csv
animationFieldnames = set()
for record in animationf:
    animationFieldnames.update(set(record.keys()))
with gzip.open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'animation_data.csv.gzip', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames = animationFieldnames)
    writer.writeheader()

    for record in animationf:
        writer.writerow(record)


# Save subject approves and rejects to file
fields = set()
for record in reject_subject:
    fields.update(set(record.keys()))
with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'reject_subjects.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=fields)
    writer.writeheader()
    for record in reject_subject:
        writer.writerow(record)

fields = set()
for record in complete_subject:
    fields.update(set(record.keys()))
with open(exp_fld + '/Data/' + min_date.strftime("%Y%m%d") + 'approve_subjects.csv', 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=fields)
    writer.writeheader()
    for record in complete_subject:
        writer.writerow(record)
