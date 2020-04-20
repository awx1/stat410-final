import csv

def subName(name, listNames):
	return [n for n in listNames if n in name] + [n for n in listNames if name in n]

def sal_stat_per_year(year, links):
	with open('/Users/alexanderxiong/Documents/STAT 410/stat410-final/Salary/format/salaries-' + year + '.csv', encoding = "latin-1") as sal:
		sal_tot = csv.reader(sal, delimiter = ',')

		sal_header = []
		sal_dict = {}
		for row in sal_tot:
			### Header Case
			if (row == ['Player', 'Link', 'Tm', 'Money', 'Signed Using', 'Year']):
				sal_header = row
				continue
			# ### Builds the player to links directory
			# if (row[1] != ''):
			# 	links[row[0]] = row[1]
			# else:
			# 	if (row[0] in links.keys()):
			# 		row[1] = links[row[0]]
			# 	else:
			# 		pass
			ye = row[-1]
			### Builds the player/team to salary/contract directory
			if ((row[0],row[2]) not in sal_dict.keys()):
				sal_dict[(row[0],row[2])] = [row[3],row[4]]
			else:
				### Reaches a repeat case
				new_contr = ''
				if (row[4] == '' and sal_dict[(row[0],row[2])][1] == ''):
					pass
				elif (row[4] != '' and sal_dict[(row[0],row[2])][1] == '') or (int(row[3]) > int(sal_dict[(row[0],row[2])][0])):
					new_contr = row[4]
				else:
					new_contr = sal_dict[(row[0],row[2])][1]
				
				new_sal = str(int(sal_dict[(row[0],row[2])][0]) + int(row[3]))
				sal_dict[(row[0],row[2])] = [new_sal, new_contr]

	with open('/Users/alexanderxiong/Documents/STAT 410/stat410-final/Salary/format/salaries-' + year + '.csv', encoding = "latin-1") as sal:
		sal_tot = csv.reader(sal, delimiter = ',')

		player_dict = {}
		for row in sal_tot:
			### Ignore header
			if (row == ['Player', 'Link', 'Tm', 'Money', 'Signed Using', 'Year']):
				sal_header = row
				continue 
			### Builds player to team directory
			if (row[0] not in player_dict.keys()):
				player_dict[row[0]] = [row[2]]
			elif (row[2] not in player_dict[row[0]]):
				player_dict[row[0]].append(row[2])
			else:
				pass
		
	with open('/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats/stats-' + year + '.csv', encoding = "latin-1") as stats:
		stats_tot = csv.reader(stats, delimiter = ',')

		stats_header = []
		stats_sal = []
		count = 0
		for row in stats_tot:
			### Ignore header
			if (row == ['Rk','Player', 'Link', 'Pos', 'Age', 'Tm', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA', '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'Year']):
				stats_header = row
				stats_header.append('Salary')
				stats_header.append('Signed Using')
				stats_sal.append(stats_header)
				continue 
			### Builds new csv
			if (row[5] == 'TOT'):
				pass
			else:
				name = row[1]
				team = row[5]
				## If exists from regular contract
				#print(name)
				if (name, team) in sal_dict.keys():
					salary = sal_dict[(name, team)][0]
					contract = sal_dict[(name, team)][1]

					new_row = row.copy()
					new_row.append(ye)
					new_row.append(salary)
					new_row.append(contract)
					stats_sal.append(new_row)
				else:
					### Player and Team do not match
					if (name in player_dict.keys()):
						for team_pay in player_dict[name]:
							salary = sal_dict[(name, team_pay)][0]
							contract = sal_dict[(name, team_pay)][1]

							new_row = row.copy()
							new_row.append(ye)
							new_row.append(salary)
							new_row.append(contract)
							stats_sal.append(new_row)
						# if name in repeats.keys():
						# 	repeats[name].append(team)
						# 	print("Played with more than one team")
						# 	print(name)
						# 	print(repeats[name])
						# else:
						# 	print("Name and team: " + str((name, team)))
						# 	print("Team(s) paying: " + str(player_dict[name]))
						# 	salary = sal_dict[(name, player_dict[name])][0]
						# 	contract = sal_dict[(name, player_dict[name])][1]

						# 	new_row = row.copy()
						# 	new_row.append(salary)
						# 	new_row.append(contract)
						# 	repeats[name] = [team]
					elif (subName(name, player_dict.keys()) != []):
						new_name = subName(name, player_dict.keys())[0]
						for team_pay in player_dict[new_name]:
							salary = sal_dict[(new_name, team_pay)][0]
							contract = sal_dict[(new_name, team_pay)][1]

							new_row = row.copy()
							new_row.append(ye)
							new_row.append(salary)
							new_row.append(contract)
							stats_sal.append(new_row)
					### Cannot find a playing player's contract	
					else:
						print(name)
						count += 1
						pass
	print(count)			
	with open("/Users/alexanderxiong/Documents/STAT 410/stat410-final/stats_sal/stats_sal-" + year + ".csv", "w", newline = "") as f:
		writer = csv.writer(f)
		writer.writerows(stats_sal)

	return links						

links = {}
links = sal_stat_per_year('201920',links)
links = sal_stat_per_year('201819',links)
links = sal_stat_per_year('201718',links)
links = sal_stat_per_year('201617',links)
links = sal_stat_per_year('201516',links)
links = sal_stat_per_year('201415',links)
links = sal_stat_per_year('201314',links)
links = sal_stat_per_year('201213',links)
