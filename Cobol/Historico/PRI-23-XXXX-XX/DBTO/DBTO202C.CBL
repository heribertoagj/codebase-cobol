      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. DBTO202C.                                            
       AUTHOR.     TIAGO MACEDO.                                        
      *================================================================*
      *                        P R I M E I T                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DBTO202C                                      *
      *    PROGRAMADOR.: TIAGO MACEDO - PRIMEIT                        *
      *    DATA........: 25/07/2022                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO.....: RESOLUCAO CADOC 8013.                        *
      *                                                                *
      *  EFETUA BATIMENTO ENTRE ARQUIVO CONTENDO DADOS DE CONTAS E TA- *
      *  RIFAS COM ARQUIVO IMAGEM DA TABELA DBTOB060 PARA OBTER        *
      *  INFORMACOES SOBRE O PAGAMENTO DAS TARIFAS.                    *
      *                                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS.....:                                              *
      *      DDNAME            I/O                   INCLUDE/BOOK      *
      *      IMAGB060           I                      DBTOWE65        *
      *      TARIFCTA           I                      DBTOWRE1        *
      *      TARIFCPL           O                      DBTOWRE1        *
      *      DBTOS23C           O                      DBTOW23C        *
      *----------------------------------------------------------------*
      *    BOOKS........:                                              *
      *    DBTOW039 - AREA PARA GERACAO DE ESTATISTICAS                *
      *    I#FRWKGE - AREA DE COMUNICACAO FRWK2999                     *
      *    I#FRWKAR - AREA PARA TRATAMENTO DE ERROS ARQUIVOS           *
      *    I#FRWKDB - COMMAREA FRWK2999 (LOG DE ERROS DB2)             *
      *    I#CKRS04 - AREA PARA CHECKPOINT / RESTART                   *
      *----------------------------------------------------------------*
      *    MODULOS......:                                              *
      *    DBTO4500 - ROTINA DE GERACAO DE ESTATISTICAS                *
      *    FRWK2999 - ROTINA DE GESTAO DE ERRO                         *
      *    BRAD0450 - BRAD PARA ABEND DE PROGRAMA                      *
      *    CKRS1000 - MODULO DE SINCRONISMO CHECKPOINT / RESTART       *
      *    CKRS0105 - REALIZA CHECKPOINT / RESTART                     *
      *    BRAD0450 - ROTINA PARA ABENDAR O PROGRAMA                   *
      *    BRAD0160 - OBTER JOBNAME                                    *
      *                                                                *
      *================================================================*
PR2710*================================================================*
.     *                        P R I M E I T                           *
.     *----------------------------------------------------------------*
.     *    DATA........: 27/10/2023                                    *
.     *----------------------------------------------------------------*
.     *    OBJETIVO.....: 27/10/2023                                   *
.     *                                                                *
.     *  AJUSTAR TRATAMENTO PARA EFETUAR A CONSOLIDACAO DE VALORES     *
PR2710*----------------------------------------------------------------*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS  COMMA.                       
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  IMAGB060 ASSIGN     TO  UT-S-IMAGB060                
                   FILE     STATUS     IS  WRK-FS-IMAGB060.             
                                                                        
           SELECT  TARIFCTA ASSIGN     TO  UT-S-TARIFCTA                
                   FILE     STATUS     IS  WRK-FS-TARIFCTA.             
                                                                        
           SELECT  TARIFCPL ASSIGN     TO  UT-S-TARIFCPL                
                   FILE     STATUS     IS  WRK-FS-TARIFCPL.             
                                                                        
           SELECT  DBTOS23C ASSIGN     TO  UT-S-DBTOS23C                
                   FILE     STATUS     IS  WRK-FS-DBTOS23C.             
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
      *                                                                 
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *    INPUT  :  TABELA DE SUMARIO DATA VENCIMENTO RECEPCAO.       *
      *              ORG. SEQUENCIAL   -   LRECL = 429                 *
      *----------------------------------------------------------------*
      *                                                                 
       FD  IMAGB060                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
      *                                                                 
       01  FD-REG-IMAGB060             PIC  X(429).                     
      *                                                                 
      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO COM INFORMACOES DE CONTAS E TARIFA        *
      *              ORG. SEQUENCIAL   -   LRECL = 300                 *
      *----------------------------------------------------------------*
      *                                                                 
       FD  TARIFCTA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
      *                                                                 
       01  FD-REG-TARIFCTA             PIC  X(300).                     
                                                                        
      *----------------------------------------------------------------*
      *    OUTPUT :  INFORMACOES DE TARIFA + CONTAS COMPLETO           *
      *              ORG. SEQUENCIAL   -   LRECL = 300                 *
      *----------------------------------------------------------------*
      *                                                                 
       FD  TARIFCPL                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
      *                                                                 
       01  FD-REG-TARIFCPL             PIC  X(300).                     
      *                                                                 
      *----------------------------------------------------------------*
      *    OUTPUT :  INFORMACOES DO LAYOUT DAS TABELAS DBTOB0DD, B0DE  *
      *              ORG. SEQUENCIAL   -   LRECL = 397                 *
      *----------------------------------------------------------------*
      *                                                                 
       FD  DBTOS23C                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD   IS STANDARD                                   
           BLOCK CONTAINS  0 RECORDS.                                   
      *                                                                 
       01  FD-REG-DBTOS23C             PIC  X(397).                     
      *                                                                 
      *================================================================*
       WORKING-STORAGE                 SECTION.                         
      *================================================================*
      *                                                                 
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** INICIO DA WORKING - DBTO0478 ***'.                      
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA ACUMULADORES'.                                    
      *----------------------------------------------------------------*
                                                                        
       77  ACU-LIDOS-IMAGB060          PIC 9(012) COMP-3  VALUE ZEROS.  
       77  ACU-LIDOS-TARIFCTA          PIC 9(012) COMP-3  VALUE ZEROS.  
       77  ACU-GRAVA-TARIFCPL          PIC 9(012) COMP-3  VALUE ZEROS.  
       77  ACU-GRAVA-DBTOS23C          PIC 9(012) COMP-3  VALUE ZEROS.  
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA VARIAVEIS AUXILIARES'.                            
      *----------------------------------------------------------------*
                                                                        
       77  WRK-ABEND-BAT               PIC S9(004)  COMP   VALUE +1111. 
       77  WRK-PROGRAMA                PIC  X(008)         VALUE        
           'DBTO202C'.                                                  
       77  WRK-DUMP-BAT                PIC  X(001)         VALUE 'S'.   
       77  WRK-LOCAL                   PIC  X(004)         VALUE ZEROS. 
       77  WRK-FRWK2999                PIC  X(008)         VALUE        
           'FRWK2999'.                                                  
       01  WRK-ABEND                   PIC  X(001)         VALUE SPACES.
           88  WRK-ABENDAR                                 VALUE 'S'.   
       77  WRK-P0160-JOB               PIC  X(008)         VALUE SPACES.
       77  WRK-P0160-VALORFAC          PIC  9(05)V99 COMP-3 VALUE ZEROS.
       77  WRK-MASCARA-SQLCODE         PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS. 
                                                                        
                                                                        
       01 WRK-CKRS0105                 PIC X(008) VALUE   'CKRS0105'.   
                                                                        
       01  WRK-MSG-IMAGB060-VAZIO      PIC X(059)          VALUE        
           'ARQUIVO IMAGB060 VAZIO, PROCESSAMENTO ENCERRADO'.           
                                                                        
       01  WRK-NOME-ARQ-TAB            PIC X(008) VALUE SPACES.         
       01  WRK-MODULO                  PIC X(008) VALUE SPACES.         
                                                                        
       01  WRK-FUNCAO                  PIC  X(064) VALUE 'F3'.          
       01  WRK-SUB-FUNCAO              PIC  X(064) VALUE 'SF3002'.      
                                                                        
       01  WRK-NCTRL-ARQ-DVDOR         PIC S9(5)V USAGE COMP-3          
                                                       VALUE ZEROS.     
       01  WRK-COMPILACAO.                                              
           03  WRK-DTCOMPILA.                                           
               05  WRK-MMCOMPILA       PIC X(002) VALUE SPACES.         
               05  FILLER              PIC X(001) VALUE SPACES.         
               05  WRK-DDCOMPILA       PIC X(002) VALUE SPACES.         
               05  FILLER              PIC X(001) VALUE SPACES.         
               05  WRK-AACOMPILA       PIC X(002) VALUE SPACES.         
           03  WRK-HRCOMPILA           PIC X(008) VALUE SPACES.         
                                                                        
       01  WRK-NULL.                                                    
           05 WRK-ROBS-CLASF-CTRL-NULL PIC S9(004) COMP    VALUE ZEROS. 
                                                                        
       01  WRK-ROBS-CLASF-AUX.                                          
           05  FILLER                  PIC  X(009) VALUE SPACES.        
           05  WRK-CONTA-COSIF         PIC  9(008) VALUE ZEROS.         
                                                                        
       01  WRK-CSERVC-CTRTE-X20        PIC  X(020)      VALUE SPACES.   
       01  FILLER                      REDEFINES  WRK-CSERVC-CTRTE-X20. 
           05  WRK-CSERVC-CTRTE        PIC  9(005).                     
           05  FILLER                  PIC  X(015).                     
                                                                        
      *                                                                 
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           'AREA PARA REDEFINICAO DE CAMPOS'.                           
      *----------------------------------------------------------------*
                                                                        
       01  WRK-S9-03                   PIC +9(003)   VALUE ZEROS.       
       01  FILLER                      REDEFINES     WRK-S9-03.         
           05  FILLER                  PIC  X(001).                     
           05  WRK-9-03                PIC  9(003).                     
                                                                        
       01  WRK-S9-10                   PIC +9(010)   VALUE ZEROS.       
       01  FILLER                      REDEFINES     WRK-S9-10.         
           05  FILLER                  PIC  X(001).                     
           05  WRK-9-10                PIC  9(010).                     
                                                                        
      *-----------------------------------------------------------------
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA CHAVE DOS ARQUIVOS *'.                          
      *-----------------------------------------------------------------
                                                                        
       01  WRK-CHV-IMAGB060.                                            
           05  WRK-CPSSOA-IMAGB060     PIC  9(010)         VALUE ZEROS. 
           05  WRK-CTPO-IMAGB060       PIC  9(003)         VALUE ZEROS. 
           05  WRK-NSEQ-IMAGB060       PIC  9(010)         VALUE ZEROS. 
                                                                        
       01  WRK-CHV-TARIFCTA.                                            
           05  WRK-CPSSOA-TARIFCTA     PIC  9(010)         VALUE ZEROS. 
           05  WRK-CTPO-TARIFCTA       PIC  9(003)         VALUE ZEROS. 
           05  WRK-NSEQ-TARIFCTA       PIC  9(010)         VALUE ZEROS. 
                                                                        
       01  WRK-NSEQ-TARIF-ANT          PIC  9(010)         VALUE ZEROS. 
                                                                        
      *-----------------------------------------------------------------
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA FILE STATUS *'.                                 
      *-----------------------------------------------------------------
                                                                        
       01  WRK-FS-IMAGB060             PIC X(002)          VALUE SPACES.
       01  WRK-FS-TARIFCTA             PIC X(002)          VALUE SPACES.
       01  WRK-FS-TARIFCPL             PIC X(002)          VALUE SPACES.
       01  WRK-FS-DBTOS23C             PIC X(002)          VALUE SPACES.
                                                                        
       01  WRK-ABERTURA                PIC X(013) VALUE 'NA ABERTURA'.  
       01  WRK-LEITURA                 PIC X(013) VALUE 'NA  LEITURA'.  
       01  WRK-GRAVACAO                PIC X(013) VALUE 'NA GRAVACAO'.  
       01  WRK-FECHAMENTO              PIC X(013) VALUE 'NO FECHAMENTO'.
                                                                        
      *-----------------------------------------------------------------
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA AUXILIAR *'.                                         
      *-----------------------------------------------------------------
       01  FILLER.                                                  
            05 WRK-IND                  PIC 9(005)  COMP-3  VALUE ZEROS. 
            05 WRK-MAX-ENCONTRADO       PIC X(001)      VALUE SPACES.    
            
       01  WRK-CONT                     PIC 9(09)   COMP-3  VALUE ZEROS.

       01  WRK-DATA-AUX                PIC X(010)          VALUE ZEROS. 
       01  FILLER                      REDEFINES  WRK-DATA-AUX.         
           05  WRK-DATA-AUX-DIA        PIC 9(002).                      
           05  WRK-DATA-AUX-P1         PIC X(001).                      
           05  WRK-DATA-AUX-MES        PIC 9(002).                      
           05  WRK-DATA-AUX-P2         PIC X(001).                      
           05  WRK-DATA-AUX-ANO        PIC 9(004).                      
                                                                        
        01  WRK-QUOC                   PIC 9(004) COMP-3   VALUE ZEROS. 
        01  WRK-RESTO                  PIC 9(004) COMP-3   VALUE ZEROS. 
        01  WRK-MES-COMP               PIC 9(002) COMP-3   VALUE ZEROS. 
        01  WRK-ANO-COMP               PIC 9(004) COMP-3   VALUE ZEROS. 
                                                                        
        01  WRK-AUX-VAJUST-TARIF-RECBR PIC S9(013)V99                   
                                                  COMP-3   VALUE ZEROS. 
        01  WRK-AUX-VSDO-TARIF-ANTER   PIC S9(013)V99                   
                                                  COMP-3   VALUE ZEROS. 
        01  WRK-AUX-VAMOTZ-PG-DVDOR    PIC S9(013)V99                   
                                                  COMP-3   VALUE ZEROS. 
        01  WRK-AUX-VLIBRC-TARIF-RECBR PIC S9(013)V99                   
                                                  COMP-3   VALUE ZEROS. 
        01  WRK-AUX-VSDO-TARIF-ATUAL   PIC S9(013)V99                   
                                                  COMP-3   VALUE ZEROS. 
                                                                        
       01  WRK-DATA-INI                PIC X(010)          VALUE ZEROS. 
       01  FILLER                      REDEFINES  WRK-DATA-INI.         
           05  WRK-DATA-INI-DIA        PIC 9(002).                      
           05  WRK-DATA-INI-P1         PIC X(001).                      
           05  WRK-DATA-INI-MES        PIC 9(002).                      
           05  WRK-DATA-INI-P2         PIC X(001).                      
           05  WRK-DATA-INI-ANO        PIC 9(004).                      
                                                                        
       01  WRK-DATA-FIM                PIC X(010)          VALUE ZEROS. 
       01  FILLER                      REDEFINES  WRK-DATA-FIM.         
           05  WRK-DATA-FIM-DIA        PIC 9(002).                      
           05  WRK-DATA-FIM-P1         PIC X(001).                      
           05  WRK-DATA-FIM-MES        PIC 9(002).                      
           05  WRK-DATA-FIM-P2         PIC X(001).                      
           05  WRK-DATA-FIM-ANO        PIC 9(004).                      
                                                                        
                                                                        
      *-----------------------------------------------------------------
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA MENSAGENS DE ERRO *'.                           
      *-----------------------------------------------------------------
                                                                        
       01  WRK-MSG-FS-ERRO.                                             
           05  FILLER                  PIC X(006) VALUE ' ERRO '.       
           05  WRK-OPERACAO            PIC X(013) VALUE SPACES.         
           05  FILLER                  PIC X(012) VALUE ' DO ARQUIVO '. 
           05  WRK-NOME-ARQ            PIC X(008) VALUE SPACES.         
           05  FILLER                  PIC X(017) VALUE                 
               ' - FILE-STATUS = '.                                     
           05  WRK-FILE-STATUS         PIC X(002) VALUE SPACES.         
           05  FILLER                  PIC X(001) VALUE ' '.            
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                  PIC  X(057) VALUE                    
           '*  AREA  COPY  PARA CALE2000 - ROTINA CALENDARIO  *'.       
      *----------------------------------------------------------------*
           COPY    'I#CALE01'.                                          
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** DBTO201C - AREA DO CALE2000 ***'.                       
      *----------------------------------------------------------------*
           COPY CALEWAAA.                                               
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA DE COMUNICACAO DO PROGRAMA FRWK2999'.                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.                                             
           05  WRK-BLOCO-INFO-ERRO     VALUE SPACES.                    
             10  WRK-CHAR-INFO-ERRO    PIC X(001)                       
                                       OCCURS 0 TO 30000 TIMES          
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DE TRATAMENTO DE ERROS DE ARQUIVO'.                    
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FRWKGARQ.                                                
           COPY 'I#FRWKAR'.                                             
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DE TRATAMENTO DE ERROS DE MODULO'.                     
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FRWKGMOD.                                                
           COPY 'I#FRWKMD'.                                             
                                                                        
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DE TRATAMENTO DE ERROS DE DB2'.                        
      *----------------------------------------------------------------*
       01  WRK-FRWKGDB2.                                                
           COPY 'I#FRWKDB'.                                             
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA DE COMUNICACAO COM DBTO4500 *'.                      
      *----------------------------------------------------------------*
                                                                        
       COPY DBTOW039.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA LAYOUT DO ARQUIVO IMAGB060 *'.                  
      *----------------------------------------------------------------*
                                                                        
       COPY DBTOWE65.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA LAYOUT DO ARQUIVO DBTOS23C *'.                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-DBTOW23C.                                                
       COPY DBTOW23C.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA LAYOUT DO ARQUIVO TARIFCTA *'.                  
      *----------------------------------------------------------------*
                                                                        
       COPY DBTOWRE1.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA CHECKPOINT/RESTART *'.                          
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#CKRS04'.                                                 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE                
           'AREA PARA DB2'.                                             
      *----------------------------------------------------------------*
           EXEC SQL                                                     
               INCLUDE SQLCA                                            
           END-EXEC.                                                    
                                                                        
      *    TABELA TCTRL_ARQ_DVDOR_TARIF                                 
           EXEC SQL                                                     
               INCLUDE DBTOB0DD                                         
           END-EXEC.                                                    
      *                                                                 
      *    TABELA TDETLH_ARQ_DVDOR_TARIF                                
           EXEC SQL                                                     
               INCLUDE DBTOB0DE                                         
           END-EXEC.                                                    
      *                                                                 
      *    TABELA DB2PRD.TCTRL_REFT_DEB                                 
           EXEC SQL                                                     
               INCLUDE DBTOB023                                         
           END-EXEC.                                                    
      *                                                                 
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE                 
           '*** FINAL  DA WORKING - DBTO202C ***'.                      
      *----------------------------------------------------------------*
      *                                                                 
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
      *                                                                 
      *----------------------------------------------------------------*
      *     ROTINA PRINCIPAL                                           *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE                  DBTOW039.                        
                                                                        
           CALL   'BRAD0160'           USING  WRK-P0160-JOB             
                                              WRK-P0160-VALORFAC.       
                                                                        
           MOVE WRK-P0160-JOB          TO DBTOW039-JOBNAME.             
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 1150-OBTER-DATA-SISTEMA.                             
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3000-PROCESSAR      UNTIL                            
                   WRK-FS-IMAGB060     EQUAL '10' OR 
                   WRK-FS-TARIFCTA     EQUAL '10'.                      
                                                                        
      *GRAVAR LAYOUT DBTOB0DD - TABELA MAE                              
           PERFORM 3600-FORMATA-DBTOB0DD.                               
           PERFORM 3400-GRAVAR-DBTOS23C.                                
                                                                        
           PERFORM 4000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     INICIALIZAR.                                               *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'CKRS1000'.                                             
                                                                        
           MOVE 'C'                    TO PARM-OP.                      
           MOVE 'DB2'                  TO PARM-SSID.                    
           MOVE SPACES                 TO PARM-PLAN.                    
                                                                        
           CALL WRK-CKRS0105           USING PARM-CKRS0105.             
                                                                        
           MOVE '0010'                 TO WRK-LOCAL.                    
           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET  ARQ-OPEN               TO TRUE.                         
           MOVE WRK-ABERTURA           TO  WRK-OPERACAO.                
                                                                        
           OPEN INPUT IMAGB060                                          
                      TARIFCTA                                          
               OUTPUT TARIFCPL                                          
                      DBTOS23C.                                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 1300-OBTER-DADOS-CALE.                               
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     TESTAR FILE STATUS.                                        *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-IMAGB060.                             
                                                                        
           PERFORM 1120-TESTAR-FS-TARIFCTA.                             
                                                                        
           PERFORM 1130-TESTAR-FS-TARIFCPL.                             
                                                                        
           PERFORM 1140-TESTAR-FS-DBTOS23C.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     TESTAR FS IMAGB060                                         *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-IMAGB060         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-IMAGB060         NOT EQUAL  '00'                  
               MOVE 'IMAGB060'         TO WRK-NOME-ARQ                  
                                          FRWKGARQ-NOME-ARQUIVO         
               MOVE WRK-FS-IMAGB060    TO WRK-FILE-STATUS               
                                          FRWKGARQ-FILE-STATUS          
               MOVE WRK-MSG-FS-ERRO    TO DBTOW039-COMENTARIO(1)        
               PERFORM 5100-DADOS-JOB-NAME                              
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9996-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     TESTAR FS TARIFCTA                                         *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-TARIFCTA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-TARIFCTA         NOT EQUAL  '00'                  
               MOVE 'TARIFCTA'         TO WRK-NOME-ARQ                  
                                          FRWKGARQ-NOME-ARQUIVO         
               MOVE WRK-FS-TARIFCTA    TO WRK-FILE-STATUS               
                                          FRWKGARQ-FILE-STATUS          
               MOVE WRK-MSG-FS-ERRO    TO DBTOW039-COMENTARIO(1)        
               PERFORM 5100-DADOS-JOB-NAME                              
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9996-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     TESTAR FS TARIFCTA                                        * 
      *----------------------------------------------------------------*
       1130-TESTAR-FS-TARIFCPL         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-TARIFCPL         NOT EQUAL  '00'                  
               MOVE 'TARIFCPL'         TO WRK-NOME-ARQ                  
                                          FRWKGARQ-NOME-ARQUIVO         
               MOVE WRK-FS-TARIFCPL    TO WRK-FILE-STATUS               
                                          FRWKGARQ-FILE-STATUS          
               MOVE WRK-MSG-FS-ERRO    TO DBTOW039-COMENTARIO(1)        
               PERFORM 5100-DADOS-JOB-NAME                              
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9996-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     TESTAR FS DBTOS23C                                        * 
      *----------------------------------------------------------------*
       1140-TESTAR-FS-DBTOS23C         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-DBTOS23C         NOT EQUAL  '00'                  
               MOVE 'DBTOS23C'         TO WRK-NOME-ARQ                  
                                          FRWKGARQ-NOME-ARQUIVO         
               MOVE WRK-FS-DBTOS23C    TO WRK-FILE-STATUS               
                                          FRWKGARQ-FILE-STATUS          
               MOVE WRK-MSG-FS-ERRO    TO DBTOW039-COMENTARIO(1)        
               PERFORM 5100-DADOS-JOB-NAME                              
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9996-ERRO-ARQ                                    
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * OBTER DATA DO SISTEMA   - PROGRAMA CALE2000                    *
      *----------------------------------------------------------------*
       1150-OBTER-DATA-SISTEMA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE CALE01-REGISTRO.                                  
                                                                        
           MOVE    'CALE0001'          TO  CALE01-ID-BLOCO.             
           MOVE    01182               TO  CALE01-TAM-BLOCO.            
           MOVE    WRK-FUNCAO          TO  CALE01-FUNCAO.               
           MOVE    WRK-SUB-FUNCAO      TO  CALE01-SUB-FUNCAO.           
                                                                        
           MOVE    'CALE2000'          TO  WRK-MODULO.                  
           CALL    WRK-MODULO          USING CALE01-REGISTRO.           
                                                                        
           IF   CALE01-COD-RETORNO     NOT EQUAL ZEROS                  
                MOVE WRK-MODULO        TO FRWKGMOD-NOME-MODULO          
                MOVE CALE01-COD-RETORNO                                 
                                       TO FRWKGMOD-COD-RETORNO          
                MOVE CALE01-COD-ERRO                                    
                                       TO FRWKGMOD-COD-ERRO             
                MOVE CALE01-COD-MENSAGEM-GMSG                           
                                       TO FRWKGMOD-COD-MENSAGEM         
                PERFORM 9997-ERRO-MOD                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1150-99-FIM.                EXIT.                                
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     CHAMAR DBTO4500                                            *
      *----------------------------------------------------------------*
       1200-CHAMAR-DBTO4500            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'DBTO202C'             TO    DBTOW039-PROGRAMA.         
                                                                        
           MOVE WHEN-COMPILED          TO  WRK-COMPILACAO.              
                                                                        
           MOVE WRK-DTCOMPILA(4:2)     TO  DBTOW039-DATA-COMP(1:2)      
           MOVE WRK-DTCOMPILA(1:2)     TO  DBTOW039-DATA-COMP(4:2)      
           MOVE '20'                   TO  DBTOW039-DATA-COMP(7:2)      
           MOVE WRK-DTCOMPILA(7:2)     TO  DBTOW039-DATA-COMP(9:2)      
           MOVE '.'                    TO  DBTOW039-DATA-COMP(3:1)      
                                           DBTOW039-DATA-COMP(6:1).     
                                                                        
           MOVE WRK-HRCOMPILA          TO  DBTOW039-HORA-COMP.          
                                                                        
           MOVE 'DBTO4500'             TO    WRK-MODULO.                
                                                                        
           CALL WRK-MODULO             USING DBTOW039.                  
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1300-OBTER-DADOS-CALE           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '0020'                 TO WRK-LOCAL.                    
           MOVE '1300-OBTER-DADOS-CALE'                                 
                                       TO FRWKGHEA-IDEN-PARAGRAFO.      
                                                                        
           INITIALIZE CALEWAAA.                                         
                                                                        
           MOVE 'SF3002'               TO CALEWAAA-FUNCAO.              
           MOVE 001                    TO CALEWAAA-VERSAO.              
           MOVE 'CALE2000'             TO WRK-MODULO.                   
                                                                        
           CALL WRK-MODULO             USING CALEWAAA.                  
                                                                        
           IF  CALEWAAA-COD-RET        NOT EQUAL ZEROS                  
                MOVE WRK-MODULO        TO FRWKGMOD-NOME-MODULO          
                MOVE CALE01-COD-RETORNO                                 
                                       TO FRWKGMOD-COD-RETORNO          
                MOVE CALE01-COD-ERRO                                    
                                       TO FRWKGMOD-COD-ERRO             
                MOVE CALE01-COD-MENSAGEM-GMSG                           
                                       TO FRWKGMOD-COD-MENSAGEM         
                PERFORM 9997-ERRO-MOD                                   
           END-IF.                                                      
                                                                        
           MOVE CALEWAAA-DT-GREGO-P-DDMMAAAA                            
                                       TO WRK-DATA-AUX.                 
                                                                        
      ***  FORMATA DATA INICIO E FIM DO MES ANTERIOR                    
                                                                        
           MOVE WRK-DATA-AUX-MES       TO WRK-MES-COMP.                 
           MOVE WRK-DATA-AUX-ANO       TO WRK-ANO-COMP.                 
                                                                        
           SUBTRACT 1                  FROM WRK-MES-COMP.               
                                                                        
           IF WRK-MES-COMP             EQUAL ZEROS                      
              MOVE 12                  TO WRK-DATA-INI-MES              
                                          WRK-DATA-FIM-MES              
              SUBTRACT 1               FROM WRK-ANO-COMP                
              MOVE WRK-ANO-COMP        TO WRK-DATA-INI-ANO              
                                          WRK-DATA-FIM-ANO              
           ELSE                                                         
              MOVE WRK-MES-COMP        TO WRK-DATA-INI-MES              
                                          WRK-DATA-FIM-MES              
              MOVE WRK-ANO-COMP        TO WRK-DATA-INI-ANO              
                                          WRK-DATA-FIM-ANO              
           END-IF.                                                      
                                                                        
           MOVE  1                     TO WRK-DATA-INI-DIA              
                                                                        
           IF  WRK-MES-COMP            EQUAL   04 OR 06 OR 09 OR 11     
               MOVE  30                TO WRK-DATA-FIM-DIA              
           ELSE                                                         
               IF  WRK-MES-COMP        EQUAL   00 OR 01 OR 03 OR 05 OR  
                                               07 OR 08 OR 10 OR 12     
                   MOVE  31            TO WRK-DATA-FIM-DIA              
               ELSE                                                     
                   DIVIDE WRK-ANO-COMP BY      4   GIVING  WRK-QUOC     
                                       REMAINDER   WRK-RESTO            
                   IF  WRK-RESTO       EQUAL   ZEROS                    
                       MOVE  29        TO WRK-DATA-FIM-DIA              
                   ELSE                                                 
                       MOVE  28        TO WRK-DATA-FIM-DIA              
                   END-IF                                               
               END-IF                                                   
           END-IF.                                                      
                                                                        
           MOVE '.'                    TO WRK-DATA-INI-P1               
                                          WRK-DATA-INI-P2               
                                          WRK-DATA-FIM-P1               
                                          WRK-DATA-FIM-P2.              
                                                                        
      ***  FORMATA DATA INICIO DO MES ATUAL                             
           MOVE 1                      TO WRK-DATA-AUX-DIA.             
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     VERIFICAR VAZIO                                            *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-IMAGB060.                                   
                                                                        
           IF  WRK-FS-IMAGB060         EQUAL '10'                       
               MOVE WRK-MSG-IMAGB060-VAZIO                              
                                       TO DBTOW039-COMENTARIO(1)        
               PERFORM 4000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
           PERFORM 2200-LER-TARIFCTA.                                   
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     LER IMAGB060                                               *
      *----------------------------------------------------------------*
       2100-LER-IMAGB060               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '0020'                 TO WRK-LOCAL.                    
           MOVE '2100-LER-IMAGB060'    TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET ARQ-READ                TO TRUE.                         
                                                                        
           READ IMAGB060.                                               
                                                                        
           IF  WRK-FS-IMAGB060         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-IMAGB060              
               GO                      TO 2100-99-FIM                   
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1110-TESTAR-FS-IMAGB060.                             
                                                                        
           MOVE FD-REG-IMAGB060        TO WE65-REGISTRO.                
                                                                        
           MOVE WE65-CPSSOA-JURID-CONTR                                 
                                       TO WRK-S9-10.                    
           MOVE WRK-9-10               TO WRK-CPSSOA-IMAGB060.          
                                                                        
           MOVE WE65-CTPO-CONTR-NEGOC  TO WRK-S9-03.                    
           MOVE WRK-9-03               TO WRK-CTPO-IMAGB060.            
                                                                        
           MOVE WE65-NSEQ-CONTR-NEGOC  TO WRK-S9-10.                    
           MOVE WRK-9-10               TO WRK-NSEQ-IMAGB060.            
                                                                        
           ADD  1                      TO ACU-LIDOS-IMAGB060.           
                                                                        
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     LER TARIFCTA                                               *
      *----------------------------------------------------------------*
       2200-LER-TARIFCTA               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '0020'                 TO WRK-LOCAL.                    
           MOVE '2200-LER-TARIFCTA'    TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET ARQ-READ                TO TRUE.                         
                                                                        
           READ TARIFCTA.                                               
                                                                        
           IF  WRK-FS-TARIFCTA         EQUAL '10'                       
               MOVE HIGH-VALUES        TO WRK-CHV-TARIFCTA              
               GO                      TO 2200-99-FIM                   
           END-IF.                                                      
                                                                        
           MOVE WRK-LEITURA            TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1120-TESTAR-FS-TARIFCTA.                             
                                                                        
           MOVE FD-REG-TARIFCTA        TO DBTOWRE1-REG.                 
           MOVE WRE1-CSERVC-CTRTE      TO WRK-CSERVC-CTRTE-X20.         
                                                                        
           MOVE WRE1-CPSSOA-JURID-CONTR                                 
                                       TO WRK-S9-10.                    
           MOVE WRK-9-10               TO WRK-CPSSOA-TARIFCTA.          
                                                                        
           MOVE WRE1-CTPO-CONTR-NEGOC  TO WRK-S9-03.                    
           MOVE WRK-9-03               TO WRK-CTPO-TARIFCTA.            
                                                                        
           MOVE WRE1-NSEQ-CONTR-NEGOC  TO WRK-S9-10.                    
           MOVE WRK-9-10               TO WRK-NSEQ-TARIFCTA.            
                                                                        
           ADD  1                      TO ACU-LIDOS-TARIFCTA.           
                                                                        
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *OBTER CHAVE DO ARQUIVO CADOC ENVIADO NO MES ANTERIOR             
           PERFORM 6000-SELECT-MAX                                      
                                                                        
           IF  WRK-CHV-IMAGB060        EQUAL WRK-CHV-TARIFCTA           
               PERFORM 3500-CALCULO-CADOC-8013                      
               PERFORM 2100-LER-IMAGB060                                
           ELSE  
               IF WRK-CONT         GREATER ZEROS 
PR2710            PERFORM 3400-GRAVAR-DBTOS23C                         
               ELSE
                   PERFORM 3100-FORMATA-TARIFCPL                 
               END-IF 

               IF  WRK-CHV-IMAGB060    LESS  WRK-CHV-TARIFCTA           
                   PERFORM 2100-LER-IMAGB060                            
               ELSE                                                     
                   PERFORM 2200-LER-TARIFCTA                            
               END-IF                                                   
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-FORMATA-TARIFCPL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WE65-CTPO-VNCLO-CONTR  TO WRE1-CTPO-VNCLO-CONTR         
           MOVE WE65-NSEQ-VNCLO-CTRTE  TO WRE1-NSEQ-VNCLO-CTRTE         
           MOVE WE65-NSEQ-IDTFD-TARIF  TO WRE1-NSEQ-IDTFD-TARIF         
                                                                        
           IF WE65-CFORMA-TARIF-PROVS-NULL                              
                                       EQUAL '?'                        
              MOVE ZEROS               TO WRE1-CFORMA-TARIF-PROVS       
           ELSE                                                         
              MOVE WE65-CFORMA-TARIF-PROVS                              
                                       TO WRE1-CFORMA-TARIF-PROVS       
           END-IF.                                                      
                                                                        
           IF WE65-DVCTO-TARIF-NULL    EQUAL '?'                        
              MOVE '01.01.0001'        TO WRE1-DVCTO-TARIF              
           ELSE                                                         
              MOVE WE65-DVCTO-TARIF    TO WRE1-DVCTO-TARIF              
           END-IF.                                                      
                                                                        
           IF WE65-DPGTO-TARIF-NULL    EQUAL '?'                        
              MOVE '01.01.0001'        TO WRE1-DPGTO-TARIF              
           ELSE                                                         
              MOVE WE65-DPGTO-TARIF    TO WRE1-DPGTO-TARIF              
           END-IF.                                                      
                                                                        
                                                                        
           IF WE65-QDEB-EFETU-CTRTE-NULL EQUAL '?'                      
              MOVE ZEROS               TO WRE1-QDEB-EFETU-CTRTE         
           ELSE                                                         
              MOVE WE65-QDEB-EFETU-CTRTE                                
                                       TO WRE1-QDEB-EFETU-CTRTE         
           END-IF.                                                      
                                                                        
           IF WE65-QREG-APURA-REMSS-NULL EQUAL '?'                      
              MOVE ZEROS               TO WRE1-QREG-APURA-REMSS         
           ELSE                                                         
              MOVE WE65-QREG-APURA-REMSS                                
                                       TO WRE1-QREG-APURA-REMSS         
           END-IF.                                                      
                                                                        
           IF WE65-VTARIF-PROVS-DEB-NULL EQUAL '?'                      
              MOVE ZEROS               TO WRE1-VTARIF-PROVS-DEB         
           ELSE                                                         
              MOVE WE65-VTARIF-PROVS-DEB                                
                                       TO WRE1-VTARIF-PROVS-DEB         
           END-IF.                                                      
                                                                        
           MOVE WRK-NSEQ-TARIFCTA      TO WRK-NSEQ-TARIF-ANT.           
                                                                        
           PERFORM 3300-GRAVAR-TARIFCPL.                                
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3200-FORMATA-SO-TARIF           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-NSEQ-TARIFCTA        NOT EQUAL WRK-NSEQ-TARIF-ANT     
                                                                        
              MOVE ZEROS               TO WRE1-CTPO-VNCLO-CONTR         
                                          WRE1-NSEQ-VNCLO-CTRTE         
                                          WRE1-NSEQ-IDTFD-TARIF         
                                          WRE1-CFORMA-TARIF-PROVS       
                                          WRE1-QDEB-EFETU-CTRTE         
                                          WRE1-QREG-APURA-REMSS         
                                          WRE1-VTARIF-PROVS-DEB         
                                                                        
              MOVE SPACES              TO WRE1-DVCTO-TARIF              
                                          WRE1-DPGTO-TARIF              
                                                                        
              PERFORM 3300-GRAVAR-TARIFCPL                              
                                                                        
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3300-GRAVAR-TARIFCPL            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '0040'                 TO WRK-LOCAL.                    
           MOVE '3300-GRAVAR-TARIFCPL' TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET ARQ-WRITE               TO TRUE.                         
                                                                        
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
                                                                        
           WRITE FD-REG-TARIFCPL       FROM DBTOWRE1-REG.               
                                                                        
           PERFORM 1130-TESTAR-FS-TARIFCPL.                             
                                                                        
           ADD 1                       TO ACU-GRAVA-TARIFCPL.           

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-GRAVAR-DBTOS23C            SECTION.                         
      *----------------------------------------------------------------*
           PERFORM 3410-OBTER-COMPL-INFO 

           MOVE '0040'                 TO WRK-LOCAL.                    
           MOVE '3400-GRAVAR-DBTOS23C' TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET ARQ-WRITE               TO TRUE.                         
                                                                        
           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.                
                                                                        
           WRITE FD-REG-DBTOS23C       FROM WRK-DBTOW23C.               
                                                                        
           PERFORM 1140-TESTAR-FS-DBTOS23C.                             
                                                                        
           ADD 1                       TO ACU-GRAVA-DBTOS23C.           
                                                                        
PR2710* ZERANDO VARIAVEIS PARA SUMARIZACAO
.          INITIALIZE WRK-AUX-VAJUST-TARIF-RECBR                        
.                     WRK-AUX-VSDO-TARIF-ANTER                          
.                     WRK-AUX-VAMOTZ-PG-DVDOR                           
PR2710                WRK-AUX-VLIBRC-TARIF-RECBR.                       
                                                                        
      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
PR2710*----------------------------------------------------------------*
.       3410-OBTER-COMPL-INFO       SECTION.
.     *----------------------------------------------------------------*
.     *CNPJ DEVEDOR                                                     
.          MOVE WRE1-CCPF-CNPJ-CTRTE   TO DBTOW23C-CCNPJ-PGDOR-FNAL.    
.          MOVE WRE1-CFLIAL-CNPJ-CTRTE TO DBTOW23C-CFLIAL-PGDOR-FNAL.   
.          MOVE WRE1-CCTRL-CPF-CTRTE   TO DBTOW23C-CCTRL-PGDOR-FNAL.    
.          
.     * INFORMACAO COMPLEMENTAR                                      
.          MOVE 'TARIFAS'              TO DBTOW23C-RCOMPL-TARIF-DVDOR.  
.                                                                       
.     *    SITUACAO DO PROCESSAMENTO DO ARQUIVO                         
.          MOVE 'DBTO202C'             TO DBTOW23C-CUSUAR-MANUT-B0DE.   
.                                                                       
.     *    DATA/HORA DA MANUTENCAO                                      
.          MOVE CALEWAAA-HH-TIMESTAMP-DB2                               
.                                      TO DBTOW23C-HMANUT-REG           
.                                      OF DBTOW23C-ENTRADA-DBTOB0DE.    
.                                                                       
.     *    MOTIVO DA MANUTENCAO                                         
.          MOVE 'PROCESSO INSERT DBTO202C'                              
.                                      TO DBTOW23C-RMOTVO-MANUT         
.                                      OF DBTOW23C-ENTRADA-DBTOB0DE.    
.          PERFORM 3510-OBTER-CTRL-REFT.
.          
.          MOVE WRK-CONTA-COSIF        TO DBTOW23C-CCTA-COSIF.          
.
.     * CODIGO FEBRABAN                                                 
.          MOVE WRK-CSERVC-CTRTE       TO DBTOW23C-CSERVC-FEBRABAN.     
.
.
.          PERFORM 3411-OBTER-TARIF-ANTERIOR.
.
.          MOVE WRK-AUX-VAMOTZ-PG-DVDOR 
.                                       TO DBTOW23C-VAMOTZ-PG-DVDOR.
.
.          MOVE WRK-AUX-VLIBRC-TARIF-RECBR
.                                      TO DBTOW23C-VLIBRC-TARIF-RECBR.
.          
.          COMPUTE WRK-AUX-VSDO-TARIF-ATUAL
.                                      =  WRK-AUX-VAMOTZ-PG-DVDOR 
.                                      +  WRK-AUX-VLIBRC-TARIF-RECBR.
.          COMPUTE DBTOW23C-VSDO-TARIF-ATUAL
.                                      =  WRK-AUX-VSDO-TARIF-ANTER
.                                      -  WRK-AUX-VSDO-TARIF-ATUAL.
.     *JUROS INCORPORADO DBTOW23C-VJURO-INC-RECBR - SEMPRE ZERO         
.          MOVE ZEROS                  TO DBTOW23C-VJURO-INC-RECBR.     
.                                                                        
.     *AJUSTES - SEMPRE ZERO                                            
.          MOVE ZEROS                  TO DBTOW23C-VAJUST-TARIF-RECBR.  
.            
.          MOVE ZEROS                  TO WRK-CONT.
.                                      
.     *----------------------------------------------------------------*
.      3410-99-FIM.                    EXIT.                                    
.     *----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.      3411-OBTER-TARIF-ANTERIOR       SECTION.
.     *----------------------------------------------------------------*
.
.     * SALDO ANTERIOR                                                   
.          MOVE WRE1-CCPF-CNPJ-CTRTE   TO CCNPJ-PGDOR-FNAL              
.                                      OF DBTOB0DE.                  
.          MOVE WRE1-CFLIAL-CNPJ-CTRTE TO CFLIAL-PGDOR-FNAL             
.                                      OF DBTOB0DE.                  
.          MOVE WRE1-CCTRL-CPF-CTRTE   TO CCTRL-PGDOR-FNAL              
.                                      OF DBTOB0DE.                            
.          MOVE WRK-MES-COMP           TO DMES-REFT-ARQ                 
.                                      OF DBTOB0DD.                  
.          MOVE WRK-ANO-COMP           TO DANO-REFT-ARQ                 
.                                      OF DBTOB0DD.                  
.          EXEC SQL                                                     
.           SELECT B.CCNPJ_PGDOR_FNAL                                  
.                , B.VSDO_TARIF_ANTER                                  
.                , B.VAMOTZ_PG_DVDOR                                   
.                , B.VLIBRC_TARIF_RECBR                                
.                , B.VJURO_INC_RECBR                                   
.                , B.VAJUST_TARIF_RECBR                                
.                , B.VSDO_TARIF_ATUAL                                  
.                , A.CSIT_PROCM_ARQ                                    
.             INTO :DBTOB0DE.CCNPJ-PGDOR-FNAL                         
.                , :DBTOB0DE.VSDO-TARIF-ANTER                       
.                , :DBTOB0DE.VAMOTZ-PG-DVDOR                        
.                , :DBTOB0DE.VLIBRC-TARIF-RECBR                     
.                , :DBTOB0DE.VJURO-INC-RECBR                        
.                , :DBTOB0DE.VAJUST-TARIF-RECBR                     
.                , :DBTOB0DE.VSDO-TARIF-ATUAL                       
.                , :DBTOB0DD.CSIT-PROCM-ARQ                         
.             FROM DB2PRD.TCTRL_ARQ_DVDOR_TARIF A                        
.                , DB2PRD.TDETLH_ARQ_DVDOR_TARIF B                       
.            WHERE A.NCTRL_ARQ_DVDOR       =  :WRK-NCTRL-ARQ-DVDOR              
.              AND A.NCTRL_ARQ_DVDOR       =  B.NCTRL_ARQ_DVDOR           
.              AND B.CCNPJ_PGDOR_FNAL      = :DBTOB0DE.CCNPJ-PGDOR-FNAL 
.              AND B.CFLIAL_PGDOR_FNAL     = :DBTOB0DE.CFLIAL-PGDOR-FNAL
.              AND B.CCTRL_PGDOR_FNAL      = :DBTOB0DE.CCTRL-PGDOR-FNAL 
.              AND A.DANO_REFT_ARQ         = :DBTOB0DD.DANO-REFT-ARQ    
.              AND A.DMES_REFT_ARQ         = :DBTOB0DD.DMES-REFT-ARQ    
.            FETCH FIRST ROW ONLY
.             WITH UR 
.          END-EXEC.                                                    
.     
.          IF ((SQLCODE                NOT EQUAL ZEROS AND +100 ) 
.          OR (SQLWARN0                EQUAL 'W'))                      
.              SET DB2-FETCH           TO TRUE                          
.              MOVE '0040'             TO FRWKGDB2-LOCAL                
.              MOVE 'DB2PRD.TCTRL_ARQ_DVDOR_TARIF'                   
.                                      TO FRWKGDB2-NOME-TABELA       
.              MOVE '3411-OBTER-TARIF-ANTERIOR'                          
.                                      TO FRWKGHEA-IDEN-PARAGRAFO    
.              MOVE SQLWARN0           TO FRWKGDB2-SQLWARN0             
.              SET WRK-ABENDAR         TO TRUE                          
.              PERFORM 9993-ERRO-DB2                                
.          END-IF                                                    
.
.          IF (SQLCODE                 EQUAL +100)              
.             MOVE ZEROS               TO VSDO-TARIF-ATUAL 
.                                      OF DBTOB0DE  
.          END-IF.                                                    
.
.          MOVE VSDO-TARIF-ATUAL        OF DBTOB0DE                            
.                                       TO DBTOW23C-VSDO-TARIF-ANTER.    
.
.     *----------------------------------------------------------------*
.       3411-99-FIM.                    EXIT.
PR2710*----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     CALCULO CADOC 8013                                         *
      *----------------------------------------------------------------*
       3500-CALCULO-CADOC-8013         SECTION.                         
      *----------------------------------------------------------------*

      *AMORTIZACAO:                                                     
      * PAGAMENTOS DO MÊS ANTERIOR - CTPO_PGT = 3                      
           IF WE65-CTPO-PGTO-TARIF     EQUAL 3                          
PR2710        COMPUTE WRK-AUX-VAMOTZ-PG-DVDOR 
                                       = WRK-AUX-VAMOTZ-PG-DVDOR
                                       + WE65-VPGTO-EFTVC
           END-IF.                                                      
                                                                                
      *LIBERACAO:                                                       
      * TARIFA ARRECADADA NO MÊS ANTERIOR COM VENCIMENTO FUTURO       
      * CTPO_PGTO_TARIF = 1                                             
           IF WE65-CTPO-PGTO-TARIF     EQUAL 1                          
PR2710        COMPUTE WRK-AUX-VLIBRC-TARIF-RECBR
                                       = WRK-AUX-VLIBRC-TARIF-RECBR
                                       + WE65-VPGTO-EFTVC
           END-IF.                                                      

           ADD 1                       TO WRK-CONT.
                                                                        
      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3510-OBTER-CTRL-REFT            SECTION.                         
      *----------------------------------------------------------------*
      *CONTA COSIF                                                      
           MOVE 741                    TO CTPO-CTRL-DEB                 
                                       OF DBTOB023                      
           MOVE 1                      TO CCTRL-REFT-DEB                
                                       OF DBTOB023                      
                                                                        
           EXEC SQL                                                     
               SELECT                                                   
                   ROBS_CLASF_CTRL                                      
               INTO                                                     
                   :DBTOB023.ROBS-CLASF-CTRL                            
                    :WRK-ROBS-CLASF-CTRL-NULL                           
               FROM                                                     
                   DB2PRD.TCTRL_REFT_DEB                                
               WHERE CTPO_CTRL_DEB    = :DBTOB023.CTPO-CTRL-DEB         
                 AND CCTRL_REFT_DEB   = :DBTOB023.CCTRL-REFT-DEB        
                 AND CINDCD_SIT_ATIVO = 'A'                             
               FETCH FIRST 1 ROWS ONLY                                  
           END-EXEC                                                     
                                                                        
           IF  SQLCODE                 NOT EQUAL ZEROS AND +100         
                                       AND -305                         
               SET DB2-SELECT          TO TRUE                          
               MOVE '0020'             TO FRWKGDB2-LOCAL                
               MOVE 'DB2PRD.TCTRL_REFT_DEB'                             
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '3510-OBTER-CTRL-REFT'                                 
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               MOVE SQLWARN0           TO FRWKGDB2-SQLWARN0             
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9993-ERRO-DB2                                    
           END-IF.                                                      
                                                                        
           IF  SQLCODE                 EQUAL +100 OR -305               
               MOVE ZEROS              TO WRK-CONTA-COSIF               
           ELSE                                                         
             IF WRK-ROBS-CLASF-CTRL-NULL LESS ZEROS                     
                MOVE ZEROS             TO WRK-CONTA-COSIF               
             ELSE                                                       
                MOVE ROBS-CLASF-CTRL   OF DBTOB023(1:17)                
                                       TO WRK-ROBS-CLASF-AUX            
             END-IF                                                     
           END-IF.                   
           
      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     FORMATAR CAMPOS DA TABELA DBTOB0DD                         *
      *----------------------------------------------------------------*
       3600-FORMATA-DBTOB0DD           SECTION.                         
      *----------------------------------------------------------------*
      *CODIGO DO SISTEMA                                                
           MOVE 'DBTO'                 TO DBTOW23C-CSIST.               
                                                                        
      *DATA DE REFERENCIA                                               
           MOVE WRK-DATA-AUX-ANO       TO DBTOW23C-DANO-REFT-ARQ.       
           MOVE WRK-DATA-AUX-MES       TO DBTOW23C-DMES-REFT-ARQ.       
                                                                        
      *CPF REMETENTE - BRADESCO                                         
           MOVE 60746948               TO DBTOW23C-CCNPJ-REMET-ARQ      
           MOVE ZEROS                  TO DBTOW23C-CFLIAL-CNPJ-REMET    
                                          DBTOW23C-CCTRL-CNPJ-REMET.    
                                                                        
      *DATA E HORA DO PROCESSAMENTO                                     
           MOVE CALEWAAA-HH-TIMESTAMP-DB2                               
                                       TO DBTOW23C-HPROCM-ARQ.          
                                                                        
      *    QUANTIDADE DE REGISTROS RECEBIDOS                            
           MOVE ACU-GRAVA-DBTOS23C     TO DBTOW23C-QREG-RECBD.          
                                                                        
      *CODIGO GERACAO ARQUIVO:                                          
      *1 = PREVIA/2 = EFETIVACAO                                        
           MOVE 1                      TO DBTOW23C-CINDCD-GERAC-ARQ     
                                                                        
      *CODIGO TRANSMISSAO DO ARQUIVO:                                   
      *T = TRANSMISSAO/R = RETRANSMISSAO                                
           MOVE 'T'                    TO DBTOW23C-CINDCD-TRNSM-ARQ.    
                                                                        
      *    SITUACAO DO PROCESSAMENTO DO ARQUIVO                         
      *    1,2,3,4,5(PENDENTE, TRANS-PREVIA,TRANS EFET,CONCLUID)        
           MOVE 1                      TO DBTOW23C-CSIT-PROCM-ARQ.      
                                                                        
      *    SITUACAO DO PROCESSAMENTO DO ARQUIVO                         
           MOVE 'DBTO202C'             TO DBTOW23C-CUSUAR-MANUT-B0DD.   
                                                                        
      *    DATA/HORA DA MANUTENCAO                                      
           MOVE CALEWAAA-HH-TIMESTAMP-DB2                               
                                       TO DBTOW23C-HMANUT-REG           
                                       OF DBTOW23C-ENTRADA-DBTOB0DD.    
                                                                        
      *    MOTIVO DA MANUTENCAO                                         
           MOVE 'PROCESSO INSERT DBTO202C'                              
                                       TO DBTOW23C-RMOTVO-MANUT         
                                       OF DBTOW23C-ENTRADA-DBTOB0DD.    
                                                                        
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     FINALIZAR                                                  *
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE '0050'                 TO WRK-LOCAL.                    
           MOVE '4000-FINALIZAR'       TO FRWKGHEA-IDEN-PARAGRAFO.      
           SET ARQ-CLOSE               TO TRUE.                         
                                                                        
           CLOSE IMAGB060                                               
                 TARIFCTA                                               
                 TARIFCPL                                               
                 DBTOS23C.                                              
                                                                        
           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.                
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 5100-DADOS-JOB-NAME.                                 
                                                                        
           MOVE 'D'                    TO PARM-OP.                      
           MOVE 'DB2'                  TO PARM-SSID.                    
           MOVE SPACES                 TO PARM-PLAN.                    
                                                                        
           CALL WRK-CKRS0105           USING PARM-CKRS0105.             
                                                                        
           PERFORM 9998-STOP-RUN.                                       
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *     DADOS JOB NAME                                             *
      *----------------------------------------------------------------*
       5100-DADOS-JOB-NAME             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'I'                    TO  DBTOW039-I-O(1).             
           MOVE 'IMAGB060'             TO  WRK-NOME-ARQ-TAB.            
           MOVE WRK-NOME-ARQ-TAB       TO  DBTOW039-DDNAME(1).          
           MOVE 'IMAGEM TABELA DBTOB060'                                
                                       TO  DBTOW039-DESCARQ(1).         
           MOVE ACU-LIDOS-IMAGB060     TO  DBTOW039-QTDEARQ(1).         
                                                                        
           MOVE 'I'                    TO  DBTOW039-I-O(2).             
           MOVE 'TARIFCTA'             TO  WRK-NOME-ARQ-TAB.            
           MOVE WRK-NOME-ARQ-TAB       TO  DBTOW039-DDNAME(2).          
           MOVE 'DADOS TARIFA + CONTAS'                                 
                                       TO  DBTOW039-DESCARQ(2).         
           MOVE ACU-LIDOS-TARIFCTA     TO  DBTOW039-QTDEARQ(2).         
                                                                        
           MOVE 'O'                    TO  DBTOW039-I-O(3).             
           MOVE 'TARIFCPL'             TO  WRK-NOME-ARQ-TAB.            
           MOVE WRK-NOME-ARQ-TAB       TO  DBTOW039-DDNAME(3).          
           MOVE 'DADOS TARIFA COMPLEMENTO'                              
                                       TO  DBTOW039-DESCARQ(3).         
           MOVE ACU-GRAVA-TARIFCPL     TO  DBTOW039-QTDEARQ(3).         
                                                                        
                                                                        
           MOVE 'O'                    TO  DBTOW039-I-O(4).             
           MOVE 'DBTOS23C'             TO  WRK-NOME-ARQ-TAB.            
           MOVE WRK-NOME-ARQ-TAB       TO  DBTOW039-DDNAME(4).          
           MOVE 'DADOS PROCESSO CADOC 8013'                             
                                       TO  DBTOW039-DESCARQ(4).         
           MOVE ACU-GRAVA-DBTOS23C     TO  DBTOW039-QTDEARQ(4).         
                                                                        
           PERFORM 1200-CHAMAR-DBTO4500.                                
                                                                        
      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       6000-SELECT-MAX                 SECTION.                         
      *----------------------------------------------------------------*
           MOVE WRK-MES-COMP           TO DMES-REFT-ARQ 
                                       OF DBTOB0DD.    
           MOVE WRK-ANO-COMP           TO DANO-REFT-ARQ 
                                       OF DBTOB0DD.    
                                                                        
           EXEC SQL                                                     
              SELECT MAX(NCTRL_ARQ_DVDOR)                               
              INTO :WRK-NCTRL-ARQ-DVDOR                                 
             FROM DB2PRD.TCTRL_ARQ_DVDOR_TARIF                          
            WHERE                                                       
                  DANO_REFT_ARQ        = :DBTOB0DD.DANO-REFT-ARQ     
              AND DMES_REFT_ARQ        = :DBTOB0DD.DMES-REFT-ARQ     
              FETCH FIRST 1 ROWS ONLY                                   
           END-EXEC.                                                    
      *                                                                *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND     
                                       -305)                            
           OR (SQLWARN0                EQUAL 'W')                       
               SET DB2-SELECT          TO TRUE                          
               MOVE '0060'             TO FRWKGDB2-LOCAL                
               MOVE 'TCTRL_ARQ_DVDOR_TARIF'                             
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE 'SELECT MAX'       TO FRWKGHEA-IDEN-PARAGRAFO       
               MOVE SQLWARN0           TO FRWKGDB2-SQLWARN0             
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9993-ERRO-DB2                                    
           END-IF.                                                      
      *                                                                 
           IF SQLCODE                  EQUAL +100 OR -305               
 .            MOVE ZEROS               TO WRK-NCTRL-ARQ-DVDOR           
 .            MOVE 'N'                 TO WRK-MAX-ENCONTRADO            
 .         ELSE                                                         
 .            MOVE 'S'                 TO WRK-MAX-ENCONTRADO            
 .         END-IF.                                                      
      *                                                                 
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9993-ERRO-DB2                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET ERRO-DB2                TO TRUE.                         
                                                                        
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.            
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.               
           MOVE WRK-FRWKGDB2           TO WRK-BLOCO-INFO-ERRO           
                                         (1:FRWKGHEA-TAM-DADOS).        
           MOVE FRWKGDB2-SQLCODE       TO WRK-MASCARA-SQLCODE.          
                                                                        
           DISPLAY ' '                                                  
           DISPLAY 'FRWKGDB2-NOME-TABELA   = ' FRWKGDB2-NOME-TABELA.    
           DISPLAY 'FRWKGDB2-STORED-PROC   = ' FRWKGDB2-STORED-PROC.    
           DISPLAY 'FRWKGDB2-LOCAL         = ' FRWKGDB2-LOCAL.          
           DISPLAY 'FRWKGDB2-SQLCODE       = ' WRK-MASCARA-SQLCODE.     
           DISPLAY 'FRWKGDB2-SQLSTATE      = ' FRWKGDB2-SQLSTATE.       
           DISPLAY 'FRWKGDB2-SQLCA         = ' FRWKGDB2-SQLCA.          
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9993-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ROTINA DE TRATAMENTO DE ERROS.                                  
      ***************************************************************** 
      *----------------------------------------------------------------*
       9994-CHAMAR-FRWK2999            SECTION.                         
      *----------------------------------------------------------------*
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           DISPLAY '************************* ' WRK-PROGRAMA            
                   ' *************************'.                        
           DISPLAY '* FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS          
      -            '       *'.                                          
           DISPLAY '*                                                   
      -            '       *'.                                          
           DISPLAY '* FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO    
                   '                             *'.                    
           DISPLAY '* FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA
                   '                       *'.                          
           DISPLAY '* FRWKGHEA-IDEN-PARAGRAFO = '                       
                   FRWKGHEA-IDEN-PARAGRAFO                              
                   ' *'.                                                
           DISPLAY '* FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS    
                   '                          *'.                       
           DISPLAY '************************* ' WRK-PROGRAMA            
                   ' *************************'.                        
           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.             
           DISPLAY '************************* ' WRK-PROGRAMA            
                   ' *************************'.                        
           DISPLAY '* PARAMETROS RETORNADOS PELO FRWK2999               
      -            '       *'.                                          
           DISPLAY '*                                                   
      -            '       *'.                                          
           DISPLAY '* FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO  
                   '                             *'.                    
           DISPLAY '* FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO     
                   '                           *'.                      
           DISPLAY '* FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM 
                   '                       *'.                          
           DISPLAY '************************* ' WRK-PROGRAMA            
                   ' *************************'.                        
                                                                        
      *----------------------------------------------------------------*
       9994-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA ABENDAR O SISTEMA.                                  
      *----------------------------------------------------------------*
       9995-ROTINA-ABEND               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.  
           DISPLAY ' '.                                                 
                                                                        
           CALL 'BRAD0450'             USING WRK-ABEND-BAT              
                                             WRK-DUMP-BAT.              
                                                                        
      *----------------------------------------------------------------*
       9995-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO DE ARQUIVO.                               
      *----------------------------------------------------------------*
       9996-ERRO-ARQ                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET ERRO-ARQUIVO            TO TRUE.                         
                                                                        
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE WRK-FRWKGARQ           TO WRK-BLOCO-INFO-ERRO           
                                       (1:FRWKGHEA-TAM-DADOS).          
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO= ' FRWKGHEA-IDEN-PARAGRAFO  
           DISPLAY 'FRWKGARQ-FILE-STATUS   = ' FRWKGARQ-FILE-STATUS.    
           DISPLAY 'FRWKGARQ-NOME-ARQUIVO  = ' FRWKGARQ-NOME-ARQUIVO.   
           DISPLAY 'FRWKGARQ-COMANDO       = ' FRWKGARQ-COMANDO.        
           DISPLAY 'WRK-LOCAL              = ' WRK-LOCAL.               
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9996-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       9997-ERRO-MOD                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET ERRO-MODULO             TO TRUE.                         
                                                                        
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE WRK-FRWKGMOD           TO WRK-BLOCO-INFO-ERRO           
                                       (1:FRWKGHEA-TAM-DADOS).          
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY 'FRWKGHEA-IDEN-PARAGRAFO= ' FRWKGHEA-IDEN-PARAGRAFO. 
           DISPLAY 'FRWKGMOD-NOME-MODULO   = ' FRWKGMOD-NOME-MODULO.    
           DISPLAY 'FRWKGMOD-COD-RETORNO   = ' FRWKGMOD-COD-RETORNO.    
           DISPLAY 'FRWKGMOD-COD-ERRO      = ' FRWKGMOD-COD-ERRO.       
           DISPLAY 'FRWKGMOD-COD-MENSAGEM  = ' FRWKGMOD-COD-MENSAGEM.   
                                                                        
           PERFORM 9999-API-ERROS.                                      
                                                                        
      *----------------------------------------------------------------*
       9997-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZACAO DO PROGRAMA                             
      *----------------------------------------------------------------*
       9998-STOP-RUN                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO.                                 
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9994-CHAMAR-FRWK2999.                                
                                                                        
           IF WRK-ABENDAR                                               
              PERFORM 9995-ROTINA-ABEND                                 
           END-IF.                                                      
                                                                        
           PERFORM 9998-STOP-RUN.                                       
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*        