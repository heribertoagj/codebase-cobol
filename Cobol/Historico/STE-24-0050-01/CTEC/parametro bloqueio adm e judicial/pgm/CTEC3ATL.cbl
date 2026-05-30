      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3ATL.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3ATL                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.LISTAR.PARÂMETRO.BLOQUEIO.           *
      *                   ADMINISTRATIVO                               *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TPARM_ENCRR_CTA_BLOQ                            CTECB028    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDII - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKD2 COMMAREA FRWK1999 (LOG DE ERROS DB2)               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
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
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*
       77  IND-1                       PIC  9(003) VALUE ZEROS COMP-3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3ATL'.

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDII.
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
                                       CTECW00I-BLOCO-RETORNO
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-BLOQ308I.

           PERFORM 2200-MOVER-SAIDA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-ACESSAR-BLOQ308I           SECTION.
      *----------------------------------------------------------------*
           MOVE CTECWDII-IND-PAGINACAO TO BLOQWC8I-INDICADOR-PAGINACAO 
           MOVE CTECWDII-PRI-CHAVE     TO BLOQWC8I-PRI-CHAVE 
           MOVE CTECWDII-ULT-CHAVE     TO BLOQWC8I-ULT-CHAVE 
      
      *    1 - TIPO OPERCAO BLOQUEIO
           MOVE 1                      TO BLOQWC8I-E-CTPO-OPERACAO

           EXEC CICS LINK
                PROGRAM (WRK-BLOQ308I)
                COMMAREA (WRK-AREA-BLOQ308I)
                LENGTH (LENGTH OF WRK-AREA-BLOQ308I)
                NOHANDLE
           END-EXEC.
      
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE '2100-ACESSAR-BLOQ308I'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      
           IF BLOQW000-COD-RETORNO     OF WRK-AREA-BLOQ308I  
                                       NOT EQUAL ZEROS AND 01
              MOVE 08                  TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0280'              TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE BLOQW000-COD-MENSAGEM 
                                       TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA

              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-MOVER-SAIDA                SECTION.
      *----------------------------------------------------------------*
           
           MOVE BLOQWC8I-PRI-CHAVE     TO CTECWDII-PRI-CHAVE 
           MOVE BLOQWC8I-ULT-CHAVE     TO CTECWDII-ULT-CHAVE 

           PERFORM                     VARYING IND-1 
                                       FROM  1
                                       BY 1
                                       UNTIL IND-1 
                                       GREATER BLOQWC8I-NUM-LINHAS 
               MOVE BLOQWC8I-E-CMOTVO-SIT-CTA(IND-1)
                                      TO CTECWDII-S-CMOVTO-SIT-CTA 
                                        (IND-1)
               MOVE BLOQWC8I-S-RJUSTF-BLOQ (IND-1)
                                      TO CTECWDII-S-RMOVTO-SIT-CTA 
                                        (IND-1)
           END-PERFORM.
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
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
