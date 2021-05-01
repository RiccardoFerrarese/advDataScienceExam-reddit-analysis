# code by Riccardo Ferrarese _ 2021 

from pmaw import PushshiftAPI

import logging
import pandas as pd

# handler = logging.StreamHandler()
# handler.setLevel(logging.INFO)

# logger = logging.getLogger('psaw')
# logger.setLevel(logging.INFO)
# logger.addHandler(handler)

# log_search = logging.getLoggerClass()


from multiprocessing import Pool
import datetime as dt
import time




COLS_SUB = ['subreddit', 'author', 'created_utc', 'url', 'num_comments', 'upvote_ratio', 'score', 'all_awardings', "total_awards_received", 'subreddit_subscribers']
COLS_COM = ['subreddit', 'author', 'created_utc', 'body', 'parent_id', 
    'link_id', 'num_comments', 'upvote_ratio', 'score', 
    'all_awardings', "total_awards_received", ]


def run(args) -> pd.DataFrame: 
    """
    Function for starts miner's' process

    Args: 
        [start, end]: [temporal intervall where we would scrap data]
        [item]: element to scrap
    Returns:
        [type]: [description]
    """

    print(args)
    start, end, item, subreddit = args

    miner = Miner(start, end, item, subreddit)
    miner.perform_search()
    return miner.read_data()

# time decorator for scrap (and save to DataFrame)
def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        t = (te-ts)*1000*60
        print(f'time for scrap: {t} minutes')
        f = open('scrap_time.txt', 'a') 
        f.write(f'{t}\n')
        f.close()
    return timed

class Miner(object):
    """ Class for Reddit Data Mining"""
    
    def __init__(self, start_epoch, end_epoch, func, subreddit) -> None:
        super().__init__()
        self.api = PushshiftAPI(rate_limit=100) 
        self.start_time = start_epoch
        self.end_time= end_epoch
        self.subreddit = subreddit
        self.data = None 
        self.func = func

    def read_data(self): 
        return self.data

    def perform_search(self):
        item = self.func 
        print(f'Start search {item}...')
        if item == 'submissions': 
            df = self.search_save_sub(self.subreddit)
            self.data = df
        if item == 'comments':
            df = self.search_save_com(self.subreddit)
            self.data = df
    
    @timeit
    def search_save_sub(self, subreddit): 
        api = self.api
        res_ = api.search_submissions(after=self.start_time,
                                before=self.end_time, 
                                subreddit=subreddit,
                                filter=COLS_SUB, 
                                #limit=2
                                )
        data = pd.DataFrame([x for x in res_])
        data.to_csv(f"./data_apr/{self.subreddit}_sub.csv")
        print(f"write {self.subreddit}_sub.csv")
        
    @timeit
    def search_save_com(self, subreddit):
        api = self.api
        res_ = api = self.api.search_comments(after=self.start_time,
                                before=self.end_time, 
                                subreddit=subreddit,
                                filter=COLS_COM, 
                                #limit=2
                            )
        data = pd.DataFrame([x for x in res_])
        data.to_csv(f"./data_apr/{self.subreddit}_com.csv")
        print(f"write {self.subreddit}_com.csv")




def main(): 

    LIST_of_SUBREDDIT = [ 
    'dogecoin',  
    'pancakeswap', 
    'Bitcoin',
    'ethereum', 
    'ethtrader', 
    'elonmusk']

    start_epoch=int(dt.datetime(2020, 1, 1).timestamp())
    end_epoch=int(dt.datetime(2021, 4, 20).timestamp())

    nargs = []
    for x in LIST_of_SUBREDDIT:
        #nargs.append( [start_epoch, end_epoch, 'submissions', x] )
        nargs.append( [start_epoch, end_epoch, 'comments', x]  )

    pool = Pool(16)
    pool.map(run, nargs)
    pool.close()
    pool.join()
    
   
if __name__ == "__main__": 
    main()