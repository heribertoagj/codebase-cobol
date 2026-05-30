      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3A1C.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3A1C                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.CONSULTAR.PARÂMETRO.BLOQUEIO.        *
      *                   ADMINISTRATIVO                               *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TPARM_ENCRR_CTA_BLOQ                            CTECB028    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDEI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
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
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3A1C'.
           05 WRK-ACHOU                PIC  X(01)  VALUE  SPACES.
           05 WRK-FIM-LISTA            PIC  X(01)  VALUE  SPACES.
           05 IND-2                    PIC  9(02)  COMP-3 VALUE ZEROS.
 
       01  WRK-AREA-NULOS.
           05 WRK-NULL-01              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-02              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-03              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-04              PIC S9(004) VALUE ZEROS COMP.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O CTEC3401   *'.
      *----------------------------------------------------------------*
       01  WRK-CTEC3401                PIC X(08)       VALUE 'CTEC3401'.
       01  WRK-AREA-CTEC3401.
           COPY CTECW00I.
           COPY CTECWDCI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O BLOQ308I   *'.
      *----------------------------------------------------------------*
       01  WRK-BLOQ308I                PIC X(08)       VALUE 'BLOQ308I'.
       01  WRK-AREA-BLOQ308I.
           COPY BLOQW000.
           COPY BLOQWC8I.

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
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.
      
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*
      
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
           COPY CTECWDEI.

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
           IF  CTECWDEI-E-CPARM-ENCRR-BLOQ  
                                       NOT NUMERIC
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTECXXX1'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           END-IF.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 2100-CONSULTA-MOTVO-DIGIT.
           
           PERFORM 2200-MOVER-SAIDA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-CONSULTA-MOTVO-DIGIT        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT CPARM_ENCRR_BLOQ
                   , CMOTVO_SIT_CTA
                   , CTPO_BLOQ_OPER
                   , CINDCD_TPO_JUDIC
                   , CSIT_PARM_ENCRR
                   , CUSUAR_INCL
                   , HINCL_REG
                   , CUSUAR_MANUT
                   , HMANUT_REG
                INTO :CTECB028.CPARM-ENCRR-BLOQ
                   , :CTECB028.CMOTVO-SIT-CTA
                         :WRK-NULL-01
                   , :CTECB028.CTPO-BLOQ-OPER
                   , :CTECB028.CINDCD-TPO-JUDIC
                         :WRK-NULL-02
                   , :CTECB028.CSIT-PARM-ENCRR
                   , :CTECB028.CUSUAR-INCL
                   , :CTECB028.HINCL-REG
                   , :CTECB028.CUSUAR-MANUT
                         :WRK-NULL-03
                   , :CTECB028.HMANUT-REG
                         :WRK-NULL-04
                 FROM DB2PRD.TPARM_ENCRR_CTA_BLOQ
                WHERE CMOTVO_SIT_CTA = :CTECB028.CMOTVO-SIT-CTA
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-SELECT         TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2100-CONSULTA-MOTVO-DIGIT' 
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF

           IF (SQLCODE                 EQUAL +100)
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0011'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           ELSE
              IF WRK-NULL-01    LESS ZEROS
                 MOVE ZEROS            TO CMOTVO-SIT-CTA 
                                       OF CTECB028
              END-IF
              
              IF WRK-NULL-02    LESS ZEROS
                 MOVE SPACES           TO CINDCD-TPO-JUDIC 
                                       OF CTECB028
              END-IF

              IF WRK-NULL-03    LESS ZEROS
                 MOVE SPACES           TO CUSUAR-MANUT
                                       OF CTECB028
              END-IF

              IF WRK-NULL-04      LESS ZEROS
                 MOVE SPACES           TO HMANUT-REG
                                       OF CTECB028
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-MOVER-SAIDA                SECTION.
      *----------------------------------------------------------------*        

           MOVE CMOTVO-SIT-CTA         OF CTECB028
                                       TO CTECWDEI-S-CMOTVO-SIT-CTA

           MOVE CINDCD-TPO-JUDIC       OF CTECB028 
                                       TO CTECWDEI-S-CINDCD-TPO-JUCIC  

           MOVE CTPO-BLOQ-OPER         OF CTECB028 
                                       TO CTECWDEI-S-CTPO-BLOQ-OPER 
            
           IF CTPO-BLOQ-OPER           OF CTECB028
                                       EQUAL 1
              MOVE 'ADMINISTRATIVO'    TO CTECWDEI-S-RTPO-BLOQ-OPER
              PERFORM 2220-OBTER-DESC-BLOQ-ADM
           ELSE 
              MOVE 'JUDICIAL'    TO CTECWDEI-S-RTPO-BLOQ-OPER
              PERFORM 2230-OBTER-DESC-BLOQ-JUDIC
           END-IF.
           
           MOVE CSIT-PARM-ENCRR        OF CTECB028 
                                       TO CTECWDEI-S-CSIT-PARM-ENCRR  
           IF CSIT-PARM-ENCRR          OF CTECB028
                                       EQUAL 'A'
              MOVE 'ATIVO'             TO CTECWDEI-S-RSIT-PARM-ENCRR 
           ELSE
              MOVE 'INATIVO'           TO CTECWDEI-S-RSIT-PARM-ENCRR 
           END-IF.

           IF CUSUAR-INCL              OF CTECB028 
                                       NOT EQUAL SPACES
              MOVE CUSUAR-INCL         OF CTECB028
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDEI-S-CUSUAR-INCL  
              PERFORM 2210-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDEI-S-IUSUAR-INCL
           ELSE
              MOVE SPACES              TO CTECWDEI-S-CUSUAR-INCL  
                                          CTECWDEI-S-IUSUAR-INCL
           END-IF.

           MOVE HINCL-REG              OF CTECB028
                                       TO CTECWDEI-S-HINCL-REG. 

           IF CUSUAR-MANUT             OF CTECB028
                                       NOT EQUAL SPACES
              MOVE CUSUAR-MANUT        OF CTECB028
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDEI-S-CUSUAR-MANUT  
              PERFORM 2210-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDEI-S-IUSUAR-MANUT 
           ELSE
              MOVE SPACES              TO CTECWDEI-S-CUSUAR-MANUT  
                                          CTECWDEI-S-IUSUAR-MANUT
           END-IF.

           MOVE HMANUT-REG             OF CTECB028
                                       TO CTECWDEI-S-HMANUT-REG.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2210-OBTER-NOME-USUARIO         SECTION.
      *----------------------------------------------------------------*
          
           EXEC CICS LINK
                PROGRAM (WRK-CTEC3401)
                COMMAREA (WRK-AREA-CTEC3401)
                LENGTH (LENGTH OF WRK-AREA-CTEC3401)
                NOHANDLE
           END-EXEC.
      
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE '2410-OBTER-NOME-USUARIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      
           IF CTECW00I-COD-RETORNO     OF WRK-AREA-CTEC3401 
                                       NOT EQUAL ZEROS
              MOVE 'NAO LOCALIZADO'    TO CTECWDCI-S-IUSUAR 
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.    
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2220-OBTER-DESC-BLOQ-ADM        SECTION. 
      *----------------------------------------------------------------*
           
           INITIALIZE BLOQWC8I-BLOCO-ENTRADA 

           MOVE 1                      TO BLOQWC8I-E-CTPO-OPERACAO.
           MOVE 'I'                    TO BLOQWC8I-INDICADOR-PAGINACAO. 
           MOVE 'N'                    TO WRK-ACHOU   
                                          WRK-FIM-LISTA   
           
           PERFORM                     UNTIL WRK-ACHOU 
                                       EQUAL 'S'
                                       OR WRK-FIM-LISTA
                                       EQUAL 'S'
               PERFORM 2221-ACESSAR-BLOQ308I

               PERFORM 2222-LOCALIZAR-DESCRICAO

           END-PERFORM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2221-ACESSAR-BLOQ308I           SECTION.       
      *----------------------------------------------------------------*

           EXEC CICS LINK
                PROGRAM (WRK-BLOQ308I)
                COMMAREA (WRK-AREA-BLOQ308I)
                LENGTH (LENGTH OF WRK-AREA-BLOQ308I)
                NOHANDLE
           END-EXEC.
      
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE '2410-OBTER-NOME-USUARIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      
           IF BLOQW000-COD-RETORNO     OF WRK-AREA-BLOQ308I  
                                       NOT EQUAL ZEROS AND 01
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA

               PERFORM 3000-FINALIZAR
           END-IF.

           IF BLOQW000-COD-RETORNO     OF WRK-AREA-BLOQ308I  
                                       EQUAL ZEROS
              MOVE 'S'                 TO WRK-FIM-LISTA
           ELSE
              MOVE 'S'                 TO BLOQWC8I-INDICADOR-PAGINACAO 
           END-IF.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2222-LOCALIZAR-DESCRICAO        SECTION.
      *----------------------------------------------------------------*
           
           PERFORM                     VARYING IND-2
                                       FROM 1
                                       BY 1
                                       UNTIL IND-2 
                                       GREATER BLOQWC8I-NUM-LINHAS 
                                       OR WRK-ACHOU 
                                       EQUAL 'S'
           
              IF BLOQWC8I-E-CMOTVO-SIT-CTA(IND-2)
                                       EQUAL CTECWDEI-S-CMOTVO-SIT-CTA 
                 MOVE BLOQWC8I-S-RJUSTF-BLOQ(IND-2)
                                       TO CTECWDEI-S-RMOTVO-SIT-CTA
                 MOVE 'S'              TO WRK-ACHOU
              END-IF
           END-PERFORM. 

      *----------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2230-OBTER-DESC-BLOQ-JUDIC      SECTION. 
      *----------------------------------------------------------------*
           
           EVALUATE CINDCD-TPO-JUDIC  OF CTECB028 
              WHEN 'V'
                 MOVE 'BLOQUEIO VALOR' 
                                      TO CTECWDEI-S-RINDCD-TPO-JUCIC 
              WHEN 'T'
                 MOVE 'BLOQUEIO TOTAL' 
                                      TO CTECWDEI-S-RINDCD-TPO-JUCIC 
              WHEN 'A'
                 MOVE 'AMBOS (TOTAL + VALOR)' 
                                      TO CTECWDEI-S-RINDCD-TPO-JUCIC 
              WHEN OTHER 
                 MOVE 'CODIGO NAO IDENTIFICADO'
                                      TO CTECWDEI-S-RINDCD-TPO-JUCIC 
           END-EVALUATE.


      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
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
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-CICS              TO TRUE.
           MOVE 16                     TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA. 
           MOVE 'CTEC0030'             TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA. 
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE EIBFN                  TO FRWKGCIC-EIBFN.
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE.
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE.
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP.
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2.
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN.
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
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
