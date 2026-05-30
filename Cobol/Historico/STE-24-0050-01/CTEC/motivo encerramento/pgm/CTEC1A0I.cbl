      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     CTEC1A0I.
       AUTHOR.                         HERIBERTO GIANNASI.

      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....:  CTEC1A0I                                     *
      *    FLUXO.......:  CTECIXXX                                     *
      *    ANALISTA....:  HERIBERTO GIANNASI                - STEFANINI*
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA COORDENADOR QUE ATENDE A FUNCAO     *
      *                   CTEC.SI.INCLUSAO.MOTIVO.ENCERRAMENTO         *
      *----------------------------------------------------------------*
      *    BOOK'S FUNCIONAIS...:                                       *
      *    CTECW00I - AREA COMUM PARA TRATAMENTO DE ERROS              *
      *    CTECWB1E - AREA PARA DADOS DE ENTRADA                       *
      *    CTECWB1I - AREA DE INTERFACE COM PROGRAMA FUNCIONAL         *
      *                                                                *
      *    BOOK'S ARQUITETURAIS:                                       *
      *    I#FRWKAQ - AREA DE COMUNICACAO DA LINKAGE                   *
      *    I#FRWK04 - AREA DE COMUNICACAO COM O GAM                    *
      *    FRWKWAAA - AREA DE ACESSO PARA DADOS DA SESSAO              *
      *    I#FRWKGE - TRATAMENTO DE ERROS - GERAL                      *
      *    I#FRWKCI - TRATAMENTO DE ERROS - CICS                       *
      *    I#FRWKMD - TRATAMENTO DE ERROS - MODULO                     *
      *----------------------------------------------------------------*
      *    MODULOS.............:                                       *
      *    FRWK1200 - GERENCIADOR DO GAM  - BOOK I#FRWK04              *
      *    FRWK1999 - TRATAMENTO DE ERROS - BOOK I#FRWKGE              *
      *    CTEC341I - INCLUIR MOTIVO DE ENCERRAMENTO                   *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050) VALUE
           '*** INICIO DA WORKING CTEC1A0I ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       77  WRK-PROGRAMA                PIC X(008) VALUE
           'CTEC1A0I'.
       77  WRK-CTEC341I                PIC X(008) VALUE
           'CTEC340I'.

       77  WRK-PARAGRAFO               PIC X(030) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE COMUNICACAO COM O GAM'.
      *----------------------------------------------------------------*
       01  WRK-FRWK1200                PIC X(008) VALUE
           'FRWK1200'.

       01  WRK-AREA-GAM.
           COPY 'I#FRWK04'.

       01  WRK-AREA-SESSAO.
           COPY FRWKWAAA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE COMUNICACAO DO PROGRAMA FRWK1999'.
      *----------------------------------------------------------------*
       01  WRK-FRWK1999                PIC X(008) VALUE
           'FRWK1999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001)
                                       OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DE TRATAMENTO DE ERRO CICS'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ENTRADA.
           COPY 'CTECWB1E'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           'AREA DO SERVICO FUNCIONAL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CTEC341I.
           COPY 'CTECW00I'.
           COPY 'CTECWB1I'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050) VALUE
           '*** FIM DA WORKING CTEC1A0I ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY 'I#FRWKAQ'.
      *
      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 1000-INICIALIZAR.
      *
           PERFORM 2000-PROCESSAR.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
           MOVE '1000-INICIALIZAR'     TO WRK-PARAGRAFO.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO.

           PERFORM 1100-RECUPERAR-DADOS-ENTRADA.

           PERFORM 1200-RECUPERAR-DADOS-SESSAO.
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-RECUPERAR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*
           MOVE '1100-RECUPERAR-DADOS-ENTRADA'
                                       TO WRK-PARAGRAFO.

           INITIALIZE  FRWKEL04-BLOCO-ENTRADA
                       FRWKEL04-BLOCO-RETORNO
                       FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE CTECWB1E-COD-LAYOUT    TO FRWKEL04-ID-BLOCO.

           MOVE LENGTH OF WRK-AREA-ENTRADA
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM.

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-ENTRADA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-RECUPERAR-DADOS-SESSAO     SECTION.
      *---------------------------------------------------------------*

           MOVE '1200-RECUPERAR-DADOS-SESSAO'
                                       TO WRK-PARAGRAFO.

           INITIALIZE  FRWKEL04-BLOCO-ENTRADA
                       FRWKEL04-BLOCO-RETORNO
                       FRWKEL04-BLOCO-ENTRADA-SAIDA.

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL.
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO.

           MOVE LENGTH OF WRK-AREA-SESSAO
                                       TO FRWKEL04-TAM-BLOCO.

           PERFORM 5000-ACESSAR-GAM.

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-SESSAO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           MOVE '2000-PROCESSAR'       TO WRK-PARAGRAFO.

           PERFORM 2100-ACESSAR-FUNCIONAL.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-ACESSAR-FUNCIONAL          SECTION.
      *----------------------------------------------------------------*
           MOVE '2100-ACESSAR-FUNCIONAL'
                                       TO WRK-PARAGRAFO.

           PERFORM 2110-VALORIZAR-AREA-FUNCIONAL.

           EXEC CICS LINK
                 PROGRAM  (WRK-CTEC341I)
                 COMMAREA (WRK-AREA-CTEC341I)
                 LENGTH   (LENGTH OF WRK-AREA-CTEC341I)
                 NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               SET  ERRO-CICS          TO TRUE
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'             TO FRWKGLAQ-COD-ERRO
               MOVE 'CTEC0030'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           EVALUATE CTECW00I-COD-RETORNO
             WHEN 00
             WHEN 08
               MOVE CTECW00I-BLOCO-RETORNO                              
                                       TO FRWKGLAQ-BLOCO-RETORNO        
             WHEN OTHER
               SET  ERRO-MODULO        TO TRUE
               MOVE 12                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0030'             TO FRWKGLAQ-COD-ERRO
               MOVE CTECW00I-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE CTECW00I-COD-RETORNO
                                       TO FRWKGMOD-COD-RETORNO
               MOVE CTECW00I-COD-ERRO  TO FRWKGMOD-COD-ERRO
               MOVE CTECW00I-COD-MENSAGEM
                                       TO FRWKGMOD-COD-MENSAGEM
               MOVE WRK-CTEC341I       TO FRWKGMOD-NOME-MODULO
               PERFORM 9998-TRATAR-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2110-VALORIZAR-AREA-FUNCIONAL   SECTION.
      *----------------------------------------------------------------*
           MOVE '2110-VALORIZAR-AREA-FUNCIONAL'
                                       TO WRK-PARAGRAFO.

           INITIALIZE CTECW00I-BLOCO-RETORNO
                      CTECWB1I-BLOCO-ENTRADA.

           MOVE CTECWB1E-BLOCO-ENTRADA   TO CTECWB1I-BLOCO-ENTRADA.

      *     MOVE FRWKWAAA-CAUTEN-SEGRC(1:9)
      *                                 TO CTECWB1I-E-CUSUAR-EXCL.
      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           EXEC CICS
                RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
               PROGRAM  (WRK-FRWK1200)
               COMMAREA (WRK-AREA-GAM)
               LENGTH   (LENGTH OF WRK-AREA-GAM)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               SET ERRO-CICS           TO TRUE
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'             TO FRWKGLAQ-COD-ERRO
               MOVE 'CTEC0030'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9998-TRATAR-ERRO
           END-IF.

           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               SET ERRO-MODULO         TO TRUE
               MOVE 12                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0050'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE FRWKEL04-COD-RETORNO
                                       TO FRWKGMOD-COD-RETORNO
               MOVE FRWKEL04-COD-ERRO  TO FRWKGMOD-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGMOD-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               PERFORM 9998-TRATAR-ERRO
           END-IF.
      *
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE WRK-PARAGRAFO          TO FRWKGHEA-IDEN-PARAGRAFO.

           EVALUATE TRUE

               WHEN ERRO-CICS
                   MOVE FRWKGCIC-TAM-LAYOUT
                                       TO FRWKGHEA-TAM-DADOS
                   MOVE EIBFN          TO FRWKGCIC-EIBFN
                   MOVE EIBRCODE       TO FRWKGCIC-EIBRCODE
                   MOVE EIBRSRCE       TO FRWKGCIC-EIBRSRCE
                   MOVE EIBRESP        TO FRWKGCIC-EIBRESP
                   MOVE EIBRESP2       TO FRWKGCIC-EIBRESP2
                   MOVE EIBTASKN       TO FRWKGCIC-EIBTASKN
                   MOVE WRK-AREA-ERRO-CICS
                                       TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS)

               WHEN ERRO-MODULO
                   MOVE FRWKGMOD-TAM-LAYOUT
                                       TO FRWKGHEA-TAM-DADOS
                   MOVE WRK-AREA-ERRO-MODULO
                                       TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS)
           END-EVALUATE.

           PERFORM 9999-API-ERROS.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC.

           IF  EIBRESP            NOT EQUAL  DFHRESP(NORMAL)
               CONTINUE
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
