       IDENTIFICATION                       DIVISION.                   
       PROGRAM-ID.                          DCOM5056.                   
       AUTHOR.                              DUARTE.                     
       DATE-WRITTEN.                        26-04-2006.                 
      *DATE-COMPILLED.                                                  
      * ALTERACAO DE ISENCAO POR OPERACOES DO CLIENTE                   
       ENVIRONMENT                          DIVISION.                   
       CONFIGURATION                        SECTION.                    
       SPECIAL-NAMES.                       DECIMAL-POINT IS COMMA.     
       DATA                                 DIVISION.                   
       WORKING-STORAGE                      SECTION.                    
       01            FILLER                 PIC  X(032)                 
                     VALUE '*** INICIO WORKING DCOM5056 ***'.           
                                                                        
       01            FILLER                 PIC  X(032)                 
                     VALUE '*** COPY I#DCOMSG           ***'.           
                                                                        
           COPY      'I#DCOMSG'.                                          
                                                                        
       01            FILLER                 PIC  X(032)                 
                     VALUE '*** COPY I#DCOM36           ***'.           
                                                                       
           COPY      'I#DCOM36'.                                         
                                                                       
       01            FILLER                 PIC  X(032)                
                     VALUE '*** COPY I#BRAD7C           ***'.          
                                                                       
           COPY      'I#BRAD7C'.                                         
                                                                       
       01            FILLER                 PIC  X(032)                
                     VALUE '*** AUXILIARES              ***'.          
                                                                       
                                                                       
       01  WRK-TIMESTAMP-ATUAL         PIC  X(026) VALUE SPACES.       
                                                                       
       01            WS-AUX.                                           
           03        WRK-ERRO               PIC S9(004)                
                     COMP                                              
                     VALUE +0.                                         
           03        WRK-MODULO             PIC  X(008)                
                     VALUE SPACES.                                     
           03        WRK-SQLCA              PIC  X(136)                
                     VALUE SPACES.                                  
           03        IND-LKG                PIC  9(02).             
           03        IND-ENT                PIC  9(02).             
           03        CT-3                   PIC  9(03) VALUE 3.     
           03        CT-S                   PIC  X(01) VALUE 'S'.   
           03        CT-FI                  PIC  X(02) VALUE 'FI'.  
           03        CT-JU                  PIC  X(02) VALUE 'JU'.  
                                                                    
           03       WS-CHV-OPER.                                    
4S2511*     05      WS-CHV-LOG-CCNPJ-CPF              PIC  9(09).   
4S2511      05      WS-CHV-LOG-CCNPJ-CPF              PIC  X(09).   
            05      WS-CHV-LOG-NSEQ-PSSOA-DESC        PIC  9(03).   
4S2511*     05      WS-CHV-LOG-CFLIAL-CNPJ            PIC  9(05).   
4S2511      05      WS-CHV-LOG-CFLIAL-CNPJ            PIC  X(04).   
            05      WS-CHV-LOG-CCTRL-CNPJ-CPF         PIC  9(02).   
            05      WS-CHV-LOG-CTPO-ISENC-DESC        PIC  9(03).   
            05      WS-CHV-LOG-DINIC-VGCIA-ISENC PIC       X(10).   
            05      WS-CHV-LOG-DANO-OPER-DESC         PIC  9(04).   
            05      WS-CHV-LOG-NSEQ-OPER-DESC         PIC  9(09).   
                                                                    
       LINKAGE                              SECTION.                
                                                                    
       01        ENTRADA-ROTEADOR.                                  
           03    ROTENT-COD-RETORNO              PIC X(04).       
           03    ROTENT-MENSAGEM                 PIC X(79).       
           03    ROTENT-RESTART                  PIC 9(05).       
           03    ROTENT-FLAG                     PIC 9(01).       
           03    ROTENT-CFUNC-BDSCO              PIC 9(09).       
           03    ROTENT-CTERM                    PIC X(08).       
4S2511*    03    ROTENT-CCNPJ-CPF                PIC 9(09).       
4S2511     03    ROTENT-CCNPJ-CPF                PIC X(09).       
4S2511*    03    ROTENT-CFLIAL-CNPJ              PIC 9(04).       
4S2511     03    ROTENT-CFLIAL-CNPJ              PIC X(04).       
           03    ROTENT-CCTRL-CNPJ-CPF           PIC 9(02).       
           03    ROTENT-CTPO-ISENC-DESC          PIC 9(03).       
           03    ROTENT-DFIM-VGCIA-ISENC         PIC X(10).       
                                                                  
           03      TABELA-OPERACOES OCCURS 40 TIMES.              
            05   ROTENT-INDIC-ALTER              PIC X(01).       
            05   ROTENT-NSEQ-PSSOA-DESC          PIC 9(03).       
            05   ROTENT-DINIC-VGCIA-ISENC        PIC X(10).       
            05   ROTENT-DANO-OPER-DESC           PIC 9(04).       
            05   ROTENT-NSEQ-OPER-DESC           PIC 9(09).       
            05   ROTENT-CCLUB                    PIC 9(10).       
            05   ROTENT-CINDCD-PSSOA-DESC        PIC X(02).       
            05   ROTENT-CAG-BCRIA                PIC X(05).       
            05   ROTENT-CCTA-BCRIA-CLI           PIC 9(13).          
                                                                     
       01        SAIDA-ROTEADOR.                                     
           03    ROTSAI-HEADER.                                      
            05   ROTSAI-COD-RETORNO              PIC X(04).          
            05   ROTSAI-MSG-RETORNO              PIC X(79).          
            05   ROTSAI-RESTART                  PIC 9(05).          
            05   ROTSAI-FLAG                     PIC 9(01).          
                                                                     
           03    ROTSAI-CONSISTENCIA.                                
            05   ROTSAI-TAMANHO-ERRO             PIC 9(01).          
            05   ROTSAI-TABELA-ERROS             OCCURS  7  TIMES.   
             07  ROTSAI-CAMPOS-ERRO              PIC 9(01).          
                                                                     
       01           LKG-ERRO-AREA           PIC  X(107).             
       01           LKG-SQLCA               PIC  X(136).             
                                                                     
       PROCEDURE                            DIVISION                 
                                            USING  ENTRADA-ROTEADOR  
                                                   SAIDA-ROTEADOR    
                                                   LKG-ERRO-AREA     
                                                   LKG-SQLCA.         
                                                                      
       000-000-INICIO                       SECTION.                  
                                                                      
           INITIALIZE SAIDA-ROTEADOR                                  
                      LKG-ERRO-AREA                                   
                      LKG-SQLCA                                       
                      36-RETORNO                                      
                      WS-CHV-OPER                                     
                      IND-LKG                                         
                      IND-ENT                                         
                      WRK-ERRO.                                       
                                                                      
      * CONSISTIR ENTRADAS                                            
                                                                      
           MOVE 1     TO ROTSAI-TAMANHO-ERRO                          
           MOVE ZEROS TO WRK-ERRO                                     
                                                                      
           IF  ROTENT-CTPO-ISENC-DESC  NOT EQUAL  001 AND 002         
               ADD 1 TO WRK-ERRO                                      
               MOVE 1                  TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
               MOVE 0495               TO ROTSAI-COD-RETORNO           
               MOVE SG-MENSAGEM(0495)  TO ROTSAI-MSG-RETORNO           
               MOVE 'APL'              TO ERR-TIPO-ACESSO              
               MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO                    
           END-IF.                                                     
                                                                       
4S2511*    IF  ROTENT-CCNPJ-CPF            EQUAL ZEROS
4S2511     IF  ROTENT-CCNPJ-CPF            EQUAL SPACES OR
4S2511         ROTENT-CCNPJ-CPF            EQUAL LOW-VALUES
               ADD 1 TO WRK-ERRO
               MOVE 2                   TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
               MOVE 0223                TO ROTSAI-COD-RETORNO          
               MOVE SG-MENSAGEM(0223)   TO ROTSAI-MSG-RETORNO          
               MOVE 'APL'               TO ERR-TIPO-ACESSO             
               MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO                    
           END-IF.                                                     
                                                                       
           IF  ROTENT-DFIM-VGCIA-ISENC     NOT NUMERIC                 
           OR  ROTENT-DFIM-VGCIA-ISENC     EQUAL ZEROS                 
               ADD 1 TO WRK-ERRO                                       
               MOVE 3                  TO ROTSAI-CAMPOS-ERRO(WRK-ERRO) 
               MOVE 0652               TO ROTSAI-COD-RETORNO           
               MOVE SG-MENSAGEM(0652)  TO ROTSAI-MSG-RETORNO           
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO                     
           END-IF.                                                      
                                                                        
           PERFORM VARYING IND-ENT FROM 1 BY 1                          
                   UNTIL IND-ENT GREATER 40                             
                   OR    TABELA-OPERACOES(IND-ENT) EQUAL SPACES         
                                                                        
                   IF  ROTENT-NSEQ-PSSOA-DESC(IND-ENT) NOT NUMERIC      
                   OR  ROTENT-NSEQ-PSSOA-DESC(IND-ENT) EQUAL ZEROS      
                   AND ROTENT-INDIC-ALTER(IND-ENT) EQUAL CT-S           
                       ADD 1 TO WRK-ERRO                                
                       MOVE 4            TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
                       MOVE 0050              TO ROTSAI-COD-RETORNO     
                       MOVE SG-MENSAGEM(0050) TO ROTSAI-MSG-RETORNO     
                       MOVE 'APL'             TO ERR-TIPO-ACESSO        
                       MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO             
                   END-IF                                               
                                                                        
                   IF  ROTENT-DINIC-VGCIA-ISENC(IND-ENT) NOT NUMERIC    
                   OR  ROTENT-DINIC-VGCIA-ISENC(IND-ENT) EQUAL ZEROS    
                   AND ROTENT-INDIC-ALTER(IND-ENT)     EQUAL CT-S       
                       ADD 1 TO WRK-ERRO                                
                       MOVE 5            TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
                       MOVE 0050              TO ROTSAI-COD-RETORNO     
                       MOVE SG-MENSAGEM(0050) TO ROTSAI-MSG-RETORNO     
                       MOVE 'APL'             TO ERR-TIPO-ACESSO        
                       MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO             
                   END-IF                                               
                                                                        
                   IF  ROTENT-DANO-OPER-DESC(IND-ENT) EQUAL ZEROS       
                   AND ROTENT-INDIC-ALTER(IND-ENT)     EQUAL CT-S       
                       ADD 1 TO WRK-ERRO                                
                       MOVE 6            TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
                       MOVE 0050              TO ROTSAI-COD-RETORNO     
                       MOVE SG-MENSAGEM(0050) TO ROTSAI-MSG-RETORNO     
                       MOVE 'APL'             TO ERR-TIPO-ACESSO        
                       MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO             
                   END-IF                                               
                                                                        
                   IF  ROTENT-NSEQ-OPER-DESC(IND-ENT) EQUAL ZEROS       
                   AND ROTENT-INDIC-ALTER(IND-ENT)     EQUAL CT-S       
                       ADD 1 TO WRK-ERRO                                
                       MOVE 7            TO ROTSAI-CAMPOS-ERRO(WRK-ERRO)
                       MOVE 0050              TO ROTSAI-COD-RETORNO     
                       MOVE SG-MENSAGEM(0050) TO ROTSAI-MSG-RETORNO     
                       MOVE 'APL'             TO ERR-TIPO-ACESSO        
                       MOVE ROTSAI-MSG-RETORNO TO ERR-TEXTO             
                   END-IF                                               
           END-PERFORM.                                                 
                                                                        
      * TRATAR NEGOCIO                                                  
                                                                        
           IF        WRK-ERRO EQUAL ZEROS                               
                     PERFORM 010-000-TRATAR-NEGOCIO                     
                             VARYING IND-ENT FROM 1 BY 1                
                             UNTIL IND-ENT GREATER 40                   
                             OR   TABELA-OPERACOES(IND-ENT) EQUAL SPACES
           END-IF.                                                      
                                                                        
      * PROCEDIMENTOS FINAIS                                            
                                                                        
           IF        36-COD-RETORNO EQUAL ZEROS                         
                     NEXT SENTENCE                                     
           ELSE                                                        
            IF       36-COD-RETORNO GREATER 90                         
            AND      36-COD-RETORNO LESS    100                        
                     MOVE WRK-SQLCA         TO LKG-SQLCA               
                     MOVE ERRO-AREA         TO LKG-ERRO-AREA           
            ELSE                                                       
                     MOVE 'DCOM5056'        TO ERR-PGM                 
                     MOVE ERRO-AREA         TO LKG-ERRO-AREA           
                     MOVE SPACES            TO LKG-SQLCA               
            END-IF                                                     
           END-IF.                                                     
                                                                       
           GOBACK.                                                     
                                                                       
       000-000-FIM.                                                    
                                                                       
                                                                       
       010-000-TRATAR-NEGOCIO               SECTION.                   
                                                                       
      * OBTER TIMESTAMP                                                
                                                                        
           MOVE  'DCOM7999'   TO  WRK-MODULO.                           
                                                                        
           CALL  WRK-MODULO   USING  WRK-TIMESTAMP-ATUAL                
                                     ERRO-AREA                          
                                     LKG-SQLCA.                         
                                                                        
4S2511*    MOVE ROTENT-CCNPJ-CPF             TO 36-CCNPJ-CPF.           
4S2511     MOVE ROTENT-CCNPJ-CPF             TO 36-CCNPJ-CPF-ST.        
           MOVE ROTENT-NSEQ-PSSOA-DESC(IND-ENT) TO 36-NSEQ-PSSOA-DESC.  
           MOVE ROTENT-DINIC-VGCIA-ISENC(IND-ENT)                       
                                               TO 36-DINIC-VGCIA-ISENC. 
           MOVE ROTENT-CTPO-ISENC-DESC          TO 36-CTPO-ISENC-DESC.  
4S2511*    MOVE ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ.       
4S2511     MOVE ROTENT-CFLIAL-CNPJ             TO 36-CFLIAL-CNPJ-ST.    
           MOVE ROTENT-CCTRL-CNPJ-CPF          TO 36-CCTRL-CNPJ-CPF.    
           MOVE ROTENT-DANO-OPER-DESC(IND-ENT) TO 36-DANO-OPER-DESC.    
           MOVE ROTENT-NSEQ-OPER-DESC(IND-ENT) TO 36-NSEQ-OPER-DESC.    
           MOVE ROTENT-CCLUB(IND-ENT)          TO 36-CCLUB.             
           MOVE ROTENT-CINDCD-PSSOA-DESC(IND-ENT)                       
                                               TO 36-CINDCD-PSSOA-DESC. 
           MOVE ROTENT-DFIM-VGCIA-ISENC        TO 36-DFIM-VGCIA-ISENC.  
           MOVE WRK-TIMESTAMP-ATUAL            TO 36-HULT-ATULZ.        
           MOVE ROTENT-CFUNC-BDSCO             TO 36-CFUNC-BDSCO.       
           MOVE ROTENT-CTERM                   TO 36-CTERM.             
                                                                        
           MOVE 'TOTAL'                        TO 36-INSTRUCAO.         
           MOVE 'DCOM6683'                     TO WRK-MODULO.           
                                                                        
           CALL      WRK-MODULO         USING                           
                                               36-ARGUMENTOS-ENTRADA    
                                               36-RETORNO               
                                               ERRO-AREA                
                                               WRK-SQLCA.               
                                                                        
           IF        36-COD-RETORNO NOT EQUAL ZEROS                     
                     MOVE   36-COD-RETORNO  TO  ROTSAI-COD-RETORNO      
                     MOVE   36-MENSAGEM     TO  ROTSAI-MSG-RETORNO      
                     MOVE   41              TO  IND-ENT                 
                     GO                     TO  010-000-FIM             
           END-IF.                                                      
                                                                        
       010-000-FIM.                                                     
