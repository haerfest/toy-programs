#!/usr/bin/env python3

class SitesParser:
    def __init__(self):
        self.sites = {}

    def parse(self, filename):
        site     = None
        machines = []
        line_nr  = 1

        f = open(filename)
        for line in f:
            thing = line.strip()

            if len(thing) == 0:
                pass
            elif thing[-1] == ':':
                if site is not None:
                    self.sites[site] = machines
                site     = thing[:-1]
                machines = []
            else:
                if site is None:
                    raise Exception('line %u: machine encountered without a site' % line_nr)
                machines.append(thing)

            line_nr += 1

        if len(machines) > 0:
            self.sites[site] = machines
        f.close()

    def dump(self):
        for (site, machines) in self.sites.items():
            print('site: %s' % site)
            for machine in machines:
                print('  machine: %s' % machine)

parser = SitesParser()
parser.parse('sites.txt')
parser.dump()
