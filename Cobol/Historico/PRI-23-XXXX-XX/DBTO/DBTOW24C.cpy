      *----------------------------------------------------------------*        
      *    ANALISTA....:   TIAGO MACEDO - PRIMEIT                      *        
      *    ANALISTA DS.:   PEDRO GRANE LOPES                           *        
      *    BOOK........:   DBTOW24C                                    *        
      *    TAMANHO.....:   0300 BYTES                                  *        
      *    DATA........:   13/09/2022                                  *        
      *----------------------------------------------------------------*        
      *    OBJETIVO....:   BOOK ENTRADA DO PROGRAMA DBTO204C           *        
      *----------------------------------------------------------------*        
      * DBTOW24C-TP-ACIONAMENTO                                        *        
      * 1 - TRANSMISSAO EFETIVA                                        *00043410
      * 2 - TRANSMISSAO PREVIA                                         *00043420
      * 3 - TRANSMISSAO RETRANSMITIR                                   *00043430
      *----------------------------------------------------------------*        
           05 DBTOW24C-REGISTRO.                                                
              10 DBTOW24C-CHAVE-MAX                  PIC 9(005).                
              10 DBTOW24C-TP-ACIONAMENTO             PIC X(001).                
              10 FILLER                              PIC X(294).                
