      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3ALI.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3ALI                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.INCLUIR.PARÂMETRO.BLOQUEIO.          *
      *                   ADMINISTRATIVO                               *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TPARM_ENCRR_CTA_BLOQ                         CTECB028    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDDI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKD2 COMMAREA FRWK1999 (LOG DE ERROS DB2)               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-PARAMETRO-EXISTE        PIC  X(01)  VALUE 'N'.
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3ALI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA DE COMUNICACAO DO PROGRAMA FRWK1999 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001) OCCURS 0
                                       TO 526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA PARA BOOK DE CONEXAO COM DB2 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-NULOS.
           05 WRK-NULL-01              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-02              PIC S9(004) VALUE ZEROS COMP.
      
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      * >> TPARM_ENCRR_CTA_BLOQ
           EXEC SQL
               INCLUDE CTECB028
           END-EXEC.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDDI.
      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR.
      *
           PERFORM 2000-PROCESSAR.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    INICIALIZACAO DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  FRWKGERR-REGISTRO
                                       FRWKGHEA-REGISTRO
                                       FRWKGDB2-REGISTRO
                                       CTECW00I-BLOCO-RETORNO
                                       OF DFHCOMMAREA.
      
           PERFORM 1100-CONSISTIR-ENTRADA.
      
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    CONSISTE DADOS DE ENTRADA                                   *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  ( CTECWDDI-E-CMOTVO-SIT-CTA  
                                       NOT NUMERIC
           OR  CTECWDDI-E-CMOTVO-SIT-CTA  
                                       EQUAL ZEROS)
           AND ( CTECWDDI-E-CINDCD-TPO-JUDIC
                                       EQUAL SPACES
           OR  CTECWDDI-E-CINDCD-TPO-JUDIC EQUAL LOW-VALUES)
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTECXXX2'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           END-IF.

           IF  CTECWDDI-E-CUSUAR-INCL  EQUAL LOW-VALUES 
           OR  CTECWDDI-E-CUSUAR-INCL  EQUAL SPACES
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTECXXX0'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           
           PERFORM 2100-VERIFICAR-PARAMETRO.

           IF WRK-PARAMETRO-EXISTE    EQUAL 'S'
              PERFORM 2300-REATIVAR-MOTVO-DIGIT
           ELSE
              PERFORM 2200-INCLUIR-MOTVO-DIGIT
           END-IF.

           MOVE ZEROS                  TO CTECW00I-COD-RETORNO 
                                       OF DFHCOMMAREA.     
           MOVE '0000'                 TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA.     

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-VERIFICAR-PARAMETRO        SECTION.
      *----------------------------------------------------------------*
           INITIALIZE CTECB028.

           PERFORM 2110-MOVER-DADOS-ENTRADA.
         
           EXEC SQL 
              SELECT CPARM_ENCRR_BLOQ
                INTO :CTECB028.CPARM-ENCRR-BLOQ
                FROM DB2PRD.TPARM_ENCRR_CTA_BLOQ
               WHERE CMOTVO_SIT_CTA   = :CTECB028.CMOTVO-SIT-CTA
                  OR CINDCD_TPO_JUDIC = :CTECB028.CINDCD-TPO-JUDIC
               FETCH FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2100-VERIFICAR-PARAMETRO' 
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.
           
           IF SQLCODE                  EQUAL ZEROS 
              MOVE 'S'                 TO WRK-PARAMETRO-EXISTE 
           ELSE
              MOVE 'N'                 TO WRK-PARAMETRO-EXISTE 
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2110-MOVER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*
           
           IF CTECWDDI-E-CMOTVO-SIT-CTA 
                                       NOT NUMERIC 
           OR CTECWDDI-E-CMOTVO-SIT-CTA 
                                       EQUAL ZEROS
              MOVE -1                  TO WRK-NULL-01
              MOVE ZEROS               TO WRK-NULL-02
                                          CMOTVO-SIT-CTA  
                                       OF CTECB028

      *       2- TIPO BLOQUEIO JUDICIAL 
              MOVE 2                   TO CTPO-BLOQ-OPER                        
                                       OF CTECB028

              MOVE CTECWDDI-E-CINDCD-TPO-JUDIC 
                                       TO CINDCD-TPO-JUDIC
                                       OF CTECB028

           ELSE 
|             MOVE -1                  TO WRK-NULL-02
              MOVE SPACES              TO CINDCD-TPO-JUDIC
                                       OF CTECB028

      *       1- TIPO BLOQUEIO ADMINISTRATIVO
              MOVE 1                   TO CTPO-BLOQ-OPER                        
                                       OF CTECB028

              MOVE ZEROS               TO WRK-NULL-01
              MOVE CTECWDDI-E-CMOTVO-SIT-CTA 
                                       TO CMOTVO-SIT-CTA  
                                       OF CTECB028
           END-IF
           
           MOVE 'A'                    TO CSIT-PARM-ENCRR 
                                       OF CTECB028

           MOVE CTECWDDI-E-CUSUAR-INCL TO CUSUAR-INCL
                                       OF CTECB028
                                       CUSUAR-MANUT
                                       OF CTECB028.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-INCLUIR-MOTVO-DIGIT        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL 
              INSERT INTO DB2PRD.TPARM_ENCRR_CTA_BLOQ
                      ( CPARM_ENCRR_BLOQ
                      , CMOTVO_SIT_CTA
                      , CTPO_BLOQ_OPER
                      , CINDCD_TPO_JUDIC
                      , CSIT_PARM_ENCRR
                      , CUSUAR_INCL
                      , HINCL_REG
                      , CUSUAR_MANUT
                      , HMANUT_REG )
              VALUES ( :CTECB028.CPARM-ENCRR-BLOQ
                     , :CTECB028.CMOTVO-SIT-CTA
                            :WRK-NULL-01
                     , :CTECB028.CTPO-BLOQ-OPER
                     , :CTECB028.CINDCD-TPO-JUDIC
                            :WRK-NULL-02
                     , :CTECB028.CSIT-PARM-ENCRR
                     , :CTECB028.CUSUAR-INCL
                     , :CTECB028.HINCL-REG
                     , NULL
                     , NULL )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2200-INCLUIR-MOTVO-DIGIT' 
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.
           
           IF SQLCODE                  NOT EQUAL ZEROS 
              MOVE 08                  TO CTECW00I-COD-RETORNO 
              MOVE '2100'              TO CTECW00I-COD-ERRO 
              MOVE 'CTEC0031'          TO CTECW00I-COD-MENSAGEM 
              PERFORM 3000-FINALIZAR 
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2300-REATIVAR-MOTVO-DIGIT       SECTION.
      *----------------------------------------------------------------*
          
           EXEC SQL 
              UPDATE DB2PRD.TPARM_ENCRR_CTA_BLOQ
                 SET CSIT_PARM_ENCRR = 'A'
                   , CUSUAR_MANUT = :CTECB028.CUSUAR-MANUT
                   , HMANUT_REG   = CURRENT_TIMESTAMP
               WHERE CPARM_ENCRR_BLOQ = :CTECB028.CPARM-ENCRR-BLOQ 
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2300-REATIVAR-MOTVO-DIGIT' 
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.
     
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   ROTINA RESPONSAVEL PELA FINALIZACAO DO PROGRAMA              *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   ROTINA PARA TRATAMENTO DE ERRO DB2                           *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-DB2               TO TRUE.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE.
           MOVE 16                     TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA. 
           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO.
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   ROTINA PARA CHAMADA DA API DE ERROS                          *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM  (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH   (LENGTH        OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
