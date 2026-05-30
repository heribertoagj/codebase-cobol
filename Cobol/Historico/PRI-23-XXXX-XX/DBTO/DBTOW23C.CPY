      *----------------------------------------------------------------*        
      *    ANALISTA....:   TIAGO MACEDO - PRIMEIT                      *        
      *    ANALISTA DS.:   PEDRO GRANE LOPES                           *        
      *    BOOK........:   DBTOW23C                                    *        
      *    TAMANHO.....:   0397 BYTES                                  *        
      *    DATA........:   25/07/2022                                  *        
      *----------------------------------------------------------------*        
      *    OBJETIVO....:   BOOK ENTRADA DO PROGRAMA DBTO200C           *        
      *----------------------------------------------------------------*        
      *                                                                *        
      *                                                                *        
      *----------------------------------------------------------------*        
           05 DBTOW23C-REGISTRO.                                                
             10 DBTOW23C-ENTRADA-DBTOB0DD.                                      
               15 DBTOW23C-NCTRL-ARQ-DVDOR-B0DD                                 
                                              PIC S9(5).                        
               15 DBTOW23C-CSIST              PIC X(4).                         
               15 DBTOW23C-DANO-REFT-ARQ      PIC S9(4).                        
               15 DBTOW23C-DMES-REFT-ARQ      PIC S9(2).                        
               15 DBTOW23C-CCNPJ-REMET-ARQ    PIC S9(9).                        
               15 DBTOW23C-CFLIAL-CNPJ-REMET  PIC S9(5).                        
               15 DBTOW23C-CCTRL-CNPJ-REMET   PIC S9(2).                        
               15 DBTOW23C-HPROCM-ARQ         PIC X(26).                        
               15 DBTOW23C-QREG-RECBD         PIC S9(9).                        
               15 DBTOW23C-CINDCD-GERAC-ARQ   PIC S9(1).                        
               15 DBTOW23C-CINDCD-TRNSM-ARQ   PIC X(1).                         
               15 DBTOW23C-CSIT-PROCM-ARQ     PIC S9(1).                        
               15 DBTOW23C-CUSUAR-MANUT-B0DD  PIC X(9).                         
               15 DBTOW23C-HMANUT-REG         PIC X(26).                        
               15 DBTOW23C-RMOTVO-MANUT       PIC X(40).                        
            10 DBTOW23C-ENTRADA-DBTOB0DE.                                       
               15 DBTOW23C-NCTRL-ARQ-DVDOR-B0DE                                 
                                              PIC S9(5).                        
               15 DBTOW23C-NDETLH-ARQ-DVDOR   PIC S9(5).                        
               15 DBTOW23C-CCTA-COSIF         PIC S9(8).                        
               15 DBTOW23C-CCNPJ-PGDOR-FNAL   PIC S9(9).                        
               15 DBTOW23C-CFLIAL-PGDOR-FNAL  PIC S9(4).                        
               15 DBTOW23C-CCTRL-PGDOR-FNAL   PIC S9(2).                        
               15 DBTOW23C-VSDO-TARIF-ANTER   PIC S9(13)V9(2).                  
               15 DBTOW23C-VAMOTZ-PG-DVDOR    PIC S9(13)V9(2).                  
               15 DBTOW23C-VLIBRC-TARIF-RECBR PIC S9(13)V9(2).          3.      
               15 DBTOW23C-VJURO-INC-RECBR    PIC S9(13)V9(2).                  
               15 DBTOW23C-VAJUST-TARIF-RECBR PIC S9(13)V9(2).          3.      
               15 DBTOW23C-VSDO-TARIF-ATUAL   PIC S9(13)V9(2).                  
               15 DBTOW23C-RCOMPL-TARIF-DVDOR PIC X(50).                        
               15 DBTOW23C-CSERVC-FEBRABAN    PIC S9(5).                        
               15 DBTOW23C-CUSUAR-MANUT-B0DE  PIC X(9).                         
               15 DBTOW23C-HMANUT-REG         PIC X(26).                        
               15 DBTOW23C-RMOTVO-MANUT       PIC X(40).                        
