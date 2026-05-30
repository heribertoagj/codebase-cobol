      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3ASL.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3ASL                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.LISTAR.PARÂMETRO.BLOQUEIO.JUDICIAL   *  
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDHI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
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
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*
       77  IND-1                       PIC  9(003) VALUE ZEROS COMP-3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3ASL'.

       01  WRK-BLQ-JUDIC.
           05 FILLER               PIC  X(31)   VALUE 
                 'VBLOQUEIO VALOR'.
           05 FILLER               PIC  X(31)   VALUE 
                 'TBLOQUEIO TOTAL'.
           05 FILLER               PIC  X(31)   VALUE 
                 'AAMBOS (TOTAL + VALOR)'.
       
       01  WRK-NUM-BLQ-JUDIC       PIC 9(01)    VALUE 3.
       01  FILLER                  REDEFINES WRK-BLQ-JUDIC.
           05 WRK-LISTA-BLQ-JUDIC  OCCURS 3.
              10 WRK-BLQ-JUDIC-COD PIC X(01).
              10 WRK-BLQ-JUDIC-DES PIC X(30).


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDHI.
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
           INITIALIZE                  CTECW00I-BLOCO-RETORNO
                                       OF DFHCOMMAREA.
      
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
                      
           PERFORM                     VARYING IND-1 
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1 
                                       GREATER WRK-NUM-BLQ-JUDIC 
              MOVE WRK-BLQ-JUDIC-COD(IND-1)
                                       TO CTECWDHI-S-CINDCD-TPO-JUDIC 
                                          (IND-1)
              MOVE WRK-BLQ-JUDIC-DES(IND-1)
                                       TO CTECWDHI-S-IINDCD-TPO-JUDIC 
                                          (IND-1)
           END-PERFORM.
           
           MOVE ZEROS                  TO CTECW00I-COD-RETORNO.               
           MOVE '0000'                 TO CTECW00I-COD-ERRO.               
           MOVE 'CTEC0020'             TO CTECW00I-COD-MENSAGEM.
                          
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
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
