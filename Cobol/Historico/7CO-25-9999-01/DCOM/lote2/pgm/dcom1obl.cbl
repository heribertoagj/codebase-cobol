      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM1OBL.
       AUTHOR.     LUIS EDUARDO.
      *================================================================*
      *                  B R Q                                         *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM1OBL                                      *
      *    PROGRAMADOR.: LUIS EDUARDO   - BRQ                          *
      *    ANALISTA....: PAULO ROBERTO  - DS - G439                    *
      *    DATA........: 12/05/2021                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: COORDENADOR PARA GERAR LISTA DE CONTRATOS     *
      *    DCOM PARA OPEN BANKING                                      *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMW002 - BOOK DE ERROS PARA COMUNICACAO ENTRE MODULOS     *
      *    DCOMWOLE - AREA DE INTERFACE COM FRWK PARA DADOS DE ENTRADA *
      *    DCOMWOLI - BOOK DE INTERFACE ENTRE COORDENADOR E FUNCIONAL  *
      *    DCOMWOLP - BOOK DE CONTROLE DE PAGINACAO DO FUNCIONAL       *
      *    DCOMWOLS - AREA DE INTERFACE COM FRWK PARA DADOS DE SAIDA   *
      *                                                                *
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKAQ COMMAREA ENTRE PROGRAMA/FRAMEWORK                  *
      *    I#FRWK04 COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)              *
      *    I#FRWKME COMMAREA MENSAGEM VARIADAS
      *    FRWKWAAA COMMAREA ENTRE PROGRAMA/FRWK1200 - DADOS SESSAO    *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKLI COMMAREA FRWK1999 (LOG DE ERROS LIVRES)            *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *    I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *    FRWK1200 - MODULO DE ACESSO AO GAM                          *
      *    DCOM3OBL - MODULO FUNCIONAL ACESSADO                        *
      *================================================================*
BETO01*                                                                *
.     *    PROGRAMADOR.:   PAULO ROBERTO SARTORATO (BETO)              *
.     *    DATA........:   16/11/2022                                  *
.     *    ALTERACAO...:   ALTERAR RC PARA A MENSAGEM DCOM0011         *
.     *                    DE 00 PARA 08                               *
BETO01*                                                                *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           '*** INICIO DA WORKING DCOM1OBL ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC X(008)  VALUE 'FRWK1999'.
           05 WRK-FRWK1200             PIC X(008)  VALUE 'FRWK1200'.
           05 WRK-PROGRAMA             PIC X(008)  VALUE 'DCOM1OBL'.
           05 WRK-DCOM3OBL             PIC X(008)  VALUE 'DCOM3OBL'.

           05 WRK-TRATAMENTO-ERROS.
              10 WRK-DEBUG             PIC X(001)  VALUE SPACES.
              10 WRK-MENSAGEM          PIC X(008)  VALUE SPACES.
              10 WRK-IDIOMA            PIC 9(005)  VALUE ZEROS.
              10 WRK-PARAMETROS        PIC X(500)  VALUE SPACES.

           05 WRK-DATA-10.
             10 WRK-DIA-10             PIC 9(002)  VALUE ZEROS.
             10 FILLER                 PIC X(001)  VALUE SPACES.
             10 WRK-MES-10             PIC 9(002)  VALUE ZEROS.
             10 FILLER                 PIC X(001)  VALUE SPACES.
             10 WRK-ANO-10             PIC 9(004)  VALUE ZEROS.

           05 WRK-DATA-08              PIC 9(008)  VALUE ZEROS.
           05 FILLER      REDEFINES    WRK-DATA-08.
             10 WRK-ANO-08             PIC 9(004).
             10 WRK-MES-08             PIC 9(002).
             10 WRK-DIA-08             PIC 9(002).

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)  VALUE
           'AREA PARA INDEXADORES'.
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC 9(005)  COMP-3 VALUE ZEROS.
       77  WRK-QTD-CONTR-COMP          PIC 9(005)  COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 526 TIMES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAJENTO DE ERRO LIVRE (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-LIVRE.
           COPY 'I#FRWKLI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE INTERFACE (GAM)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWK1200.
           COPY 'I#FRWK04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA GRAVAR MENSAGENS VARIAVEIS  *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWKME.
           COPY 'I#FRWKME'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA PARA BUSCAR DADOS DE SESSAO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWKAAA.
           COPY FRWKWAAA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE PARA DADOS DE PERSISTENCIA'.
      *----------------------------------------------------------------*
       01  WRK-DCOMWOLP.
           COPY DCOMWOLP.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-DCOMWOLE.
           COPY DCOMWOLE.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE PARA DADOS DE SAIDA'.
      *----------------------------------------------------------------*
       01  WRK-DCOMWOLS.
           COPY DCOMWOLS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE INTERFACE COM SERVICO FUNCIONAL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DCOM3OBL.
           COPY DCOMW002.
           COPY DCOMWOLI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           '*** FIM DA WORKING DCOM1OBL ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY 'I#FRWKAQ'.

      *================================================================*
       PROCEDURE                       DIVISION  USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR
           .
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO

           MOVE 'N'                    TO WRK-DEBUG

           PERFORM 6000-OBTER-DADOS-ENTRADA

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA

           PERFORM 6100-RECUPERA-DADOS-SESSAO
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

            MOVE '1100-CONSISTIR-DADOS-ENTRADA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

DEBUG       IF  WRK-DEBUG              EQUAL 'S'
=               STRING DCOMWOLE-BLOCO-LISTACONTR
=               '/' DCOMWOLE-E-CCTRL-CPF-CNPJ
=               DELIMITED BY SIZE      INTO WRK-PARAMETROS
=               PERFORM 9250-ERRO-DEBUG
DEBUG       END-IF

           IF (DCOMWOLE-E-AGENCIA           NOT NUMERIC)
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0003'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (DCOMWOLE-E-CONTA             NOT NUMERIC)
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0020'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0030'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (DCOMWOLE-E-CSEQ-TTLAR        NOT NUMERIC)
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0030'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0031'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

4S2511*    IF (DCOMWOLE-E-CCPF-CNPJ         NOT NUMERIC)
4S2511*        MOVE 08                      TO FRWKGLAQ-COD-RETORNO
4S2511*        MOVE '0040'                  TO FRWKGLAQ-COD-ERRO
4S2511*        MOVE 'DCOM0023'              TO FRWKGLAQ-COD-MENSAGEM
4S2511*        PERFORM 3000-FINALIZAR
4S2511*    END-IF
4S2511     IF  DCOMWOLE-E-CCPF-CNPJ      EQUAL SPACES OR
4S2511         DCOMWOLE-E-CCPF-CNPJ      EQUAL LOW-VALUES
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0023'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

4S2511*    IF (DCOMWOLE-E-CFLIAL-CNPJ       NOT NUMERIC)
4S2511*        MOVE 08                      TO FRWKGLAQ-COD-RETORNO
4S2511*        MOVE '0050'                  TO FRWKGLAQ-COD-ERRO
4S2511*        MOVE 'DCOM0033'              TO FRWKGLAQ-COD-MENSAGEM
4S2511*        PERFORM 3000-FINALIZAR
4S2511*    END-IF
4S2511     IF  DCOMWOLE-E-CFLIAL-CNPJ    EQUAL SPACES OR
4S2511         DCOMWOLE-E-CFLIAL-CNPJ    EQUAL LOW-VALUES
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0050'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0033'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (DCOMWOLE-E-CCTRL-CPF-CNPJ    NOT NUMERIC)
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0060'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0034'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF  DCOMWOLE-E-DATA-INICIO       EQUAL LOW-VALUES
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0070'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0018'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF  DCOMWOLE-E-DATA-INICIO       NOT EQUAL SPACES
               MOVE DCOMWOLE-E-DATA-INICIO  TO WRK-DATA-10
               MOVE WRK-DIA-10              TO WRK-DIA-08
               MOVE WRK-MES-10              TO WRK-MES-08
               MOVE WRK-ANO-10              TO WRK-ANO-08
               IF (WRK-DATA-08              NOT NUMERIC)
               OR (WRK-DIA-08               EQUAL ZEROS)
               OR (WRK-DIA-08               GREATER 31)
               OR (WRK-MES-08               EQUAL ZEROS)
               OR (WRK-MES-08               GREATER 12)
               OR (WRK-ANO-08               EQUAL ZEROS)
                   MOVE 08                  TO FRWKGLAQ-COD-RETORNO
                   MOVE '0080'              TO FRWKGLAQ-COD-ERRO
                   MOVE 'DCOM0007'          TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF

           IF  DCOMWOLE-E-DATA-FIM          EQUAL LOW-VALUES
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0090'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0019'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF  DCOMWOLE-E-DATA-FIM          NOT EQUAL SPACES
               MOVE DCOMWOLE-E-DATA-FIM     TO WRK-DATA-10
               MOVE WRK-DIA-10              TO WRK-DIA-08
               MOVE WRK-MES-10              TO WRK-MES-08
               MOVE WRK-ANO-10              TO WRK-ANO-08
               IF (WRK-DATA-08              NOT NUMERIC)
               OR (WRK-DIA-08               EQUAL ZEROS)
               OR (WRK-DIA-08               GREATER 31)
               OR (WRK-MES-08               EQUAL ZEROS)
               OR (WRK-MES-08               GREATER 12)
               OR (WRK-ANO-08               EQUAL ZEROS)
                   MOVE 08                  TO FRWKGLAQ-COD-RETORNO
                   MOVE '0100'              TO FRWKGLAQ-COD-ERRO
                   MOVE 'DCOM0007'          TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF
           .
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
             WHEN FRWKWAAA-CONSULTA-INICIAL
             WHEN FRWKWAAA-PRIMEIRO-BLOCO
                  PERFORM 2100-CONSULTA-INICIAL
             WHEN FRWKWAAA-BLOCO-SEGUINTE
                  PERFORM 2200-PAGINAR
             WHEN FRWKWAAA-ULTIMO-BLOCO
                  MOVE 16              TO FRWKGLAQ-COD-RETORNO
                  MOVE '0110'          TO FRWKGLAQ-COD-ERRO
                  MOVE 'DCOM0021'      TO FRWKGLAQ-COD-MENSAGEM
                  MOVE '2000-PROCESSAR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
                  MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
                  MOVE SPACES          TO WRK-PARAMETROS
                  PERFORM 9200-ERRO-LIVRE
             WHEN FRWKWAAA-BLOCO-ANTERIOR
                  MOVE 16              TO FRWKGLAQ-COD-RETORNO
                  MOVE '0120'          TO FRWKGLAQ-COD-ERRO
                  MOVE 'DCOM0021'      TO FRWKGLAQ-COD-MENSAGEM
                  MOVE '2000-PROCESSAR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
                  MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
                  MOVE SPACES          TO WRK-PARAMETROS
                  PERFORM 9200-ERRO-LIVRE
             WHEN OTHER
                  MOVE 16              TO FRWKGLAQ-COD-RETORNO
                  MOVE '0130'          TO FRWKGLAQ-COD-ERRO
                  MOVE 'DCOM0021'      TO FRWKGLAQ-COD-MENSAGEM
                  MOVE '2000-PROCESSAR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO WRK-MENSAGEM
                  MOVE FRWKWAAA-WINFO-DADOS-IDIOM
                                       TO WRK-IDIOMA
                  MOVE SPACES          TO WRK-PARAMETROS
                  PERFORM 9200-ERRO-LIVRE
           END-EVALUATE

           MOVE DCOMW002-BLOCO-RETORNO TO FRWKGLAQ-BLOCO-RETORNO
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PRIMEIRA CONSULTA DO PROGRAMA                   *
      *----------------------------------------------------------------*
       2100-CONSULTA-INICIAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-FORMATA-COMMAREA

           PERFORM 2300-ACESSAR-FUNCIONAL

           PERFORM 2400-MOVER-DADOS-SAIDA

           PERFORM 6200-INFORMAR-BLOCO-SAIDA

           IF  (DCOMW002-COD-RETORNO   EQUAL 01)
               IF (FRWKWAAA-CONSULTA-INICIAL )
                  PERFORM 6300-CRIAR-GAM-PERSISTENTE
               ELSE
                  PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PAGINAR A CONSULTA.                             *
      *----------------------------------------------------------------*
       2200-PAGINAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 6600-CONSULTAR-GAM-PERSISTENTE

           PERFORM 6700-CONSISTIR-CHAVES

           PERFORM 2210-FORMATA-COMMAREA

           PERFORM 2220-FORMATA-CHAVES

           PERFORM 2300-ACESSAR-FUNCIONAL

           PERFORM 2400-MOVER-DADOS-SAIDA

           PERFORM 6200-INFORMAR-BLOCO-SAIDA

           PERFORM 6400-ATUALIZAR-GAM-PERSISTENTE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR O COPYBOOK DO SERVICO FUNCIONAL        *
      *----------------------------------------------------------------*
       2210-FORMATA-COMMAREA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  DCOMWOLI-BLOCO-ENTRADA
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO DCOMWOLI-INDICADOR-PAGINACAO

           MOVE DCOMWOLE-BLOCO-ENTRADA TO DCOMWOLI-BLOCO-ENTRADA.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS CAMPOS CHAVES PARA VALIDACAO        *
      *----------------------------------------------------------------*
       2220-FORMATA-CHAVES             SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMWOLP-CHAVE-INI     TO DCOMWOLI-CHAVE-INI

           MOVE DCOMWOLP-CHAVE-FIM     TO DCOMWOLI-CHAVE-FIM
           .
      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSO AO MODULO FUNCIONAL.                     *
      *----------------------------------------------------------------*
       2300-ACESSAR-FUNCIONAL          SECTION.
      *----------------------------------------------------------------*

            MOVE '2300-ACESSAR-FUNCIONAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

DEBUG       IF  WRK-DEBUG              EQUAL 'S'
=               STRING DCOMWOLI-BLOCO-LISTACONTR
=               'IPG=' DCOMWOLI-INDICADOR-PAGINACAO
=               'CHI=' DCOMWOLI-CHAVE-INI
=               'CHF=' DCOMWOLI-CHAVE-FIM
=               DELIMITED BY SIZE      INTO WRK-PARAMETROS
=               PERFORM 9250-ERRO-DEBUG
DEBUG      END-IF

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3OBL)
                COMMAREA (WRK-AREA-DCOM3OBL)
                LENGTH   (LENGTH OF WRK-AREA-DCOM3OBL)
                NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0140'             TO FRWKGLAQ-COD-ERRO
               PERFORM 9400-ERRO-CICS
           END-IF

DEBUG       IF  WRK-DEBUG              EQUAL 'S'
=               STRING 'R-3OBL-MSG='DCOMW002-COD-MENSAGEM
=               'CD=' DCOMW002-COD-RETORNO
=               'LC=' DCOMW002-COD-ERRO
=               'PR=' DCOMW002-PARAMETRO
=               DELIMITED BY SIZE      INTO WRK-PARAMETROS
=               PERFORM 9250-ERRO-DEBUG
DEBUG       END-IF

           IF  DCOMW002-COD-MENSAGEM   EQUAL 'DCOM0035'
               MOVE 1                  TO FRWKGMEN-QUANT-MENS
               MOVE DCOMW002-COD-MENSAGEM
                                       TO FRWKGMEN-COD-MENS(1)
               MOVE DCOMW002-PARAMETRO TO FRWKGMEN-PARAMETROS(1)
               PERFORM 9100-CRIAR-BLOCO-MENSAGEM
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE DCOMW002-COD-ERRO  TO FRWKGLAQ-COD-ERRO
               MOVE DCOMW002-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9300-ERRO-MODULO
           END-IF

           EVALUATE DCOMW002-COD-RETORNO
             WHEN 00
             WHEN 01
               CONTINUE
             WHEN 08
               MOVE DCOMW002-COD-RETORNO
                                       TO FRWKGLAQ-COD-RETORNO
               MOVE DCOMW002-COD-ERRO  TO FRWKGLAQ-COD-ERRO
               MOVE DCOMW002-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
             WHEN OTHER
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0150'             TO FRWKGLAQ-COD-ERRO
               MOVE DCOMW002-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-DCOM3OBL       TO FRWKGMOD-NOME-MODULO
               MOVE DCOMW002-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
               PERFORM 9300-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR A AREA DE SAIDA.            *
      *----------------------------------------------------------------*
       2400-MOVER-DADOS-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE DCOMWOLS-BLOCO-SAIDA.

           IF  DCOMWOLI-S-QTDE-CONTRATO EQUAL ZEROS
BETO01         MOVE 8                       TO FRWKGLAQ-COD-RETORNO
               MOVE '0160'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0011'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           MOVE DCOMWOLI-S-QTDE-CONTRATO
                                       TO DCOMWOLS-S-QTDE-CONTRATO
                                          WRK-QTD-CONTR-COMP

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER WRK-QTD-CONTR-COMP

               MOVE DCOMWOLI-S-OCORRENCIAS(WRK-IND)
                                    TO DCOMWOLS-S-OCORRENCIAS(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                RETURN
           END-EXEC
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUAR CHAMADA AO MODULO FRWK1200 - GAM                    *
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM                SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-FRWK1200)
                LENGTH   (LENGTH OF WRK-AREA-FRWK1200)
                NOHANDLE
           END-EXEC

           IF (EIBRESP             NOT EQUAL DFHRESP(NORMAL))
               MOVE '0170'             TO FRWKGLAQ-COD-ERRO
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF

           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE 16                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0180'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               MOVE FRWKEL04-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
               MOVE '5000-ACESSAR-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6000-OBTER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET  FRWKEL04-FUN-CONSULTAR TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWOLE-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWOLE-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-DCOMWOLE
           .
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RECUPERAR DADOS DE SESSAO.                      *
      *----------------------------------------------------------------*
       6100-RECUPERA-DADOS-SESSAO      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE 'FRWKEL83'             TO FRWKEL04-ID-BLOCO
           MOVE FRWKWAAA-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-AREA-FRWKAAA
           .
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FORMATAR OS DADOS DE SAUDA                      *
      *----------------------------------------------------------------*
       6200-INFORMAR-BLOCO-SAIDA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-CONSULTAR TO TRUE
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWOLS-COD-LAYOUT    TO FRWKEL04-ID-BLOCO

           MOVE LENGTH OF WRK-DCOMWOLS
                                       TO FRWKEL04-TAM-BLOCO
                                       DCOMWOLS-TAM-LAYOUT
           MOVE WRK-DCOMWOLS           TO FRWKEL04-BUFFER
                                                  (1:FRWKEL04-TAM-BLOCO)

           PERFORM 5000-ACESSAR-GAM
           .
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CRIACAO DO BLOCO DE PERSISTENCIA.               *
      *----------------------------------------------------------------*
       6300-CRIAR-GAM-PERSISTENTE      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-RESTRITO  TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE

           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM
           .
      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ATUALIZACAO DO BLOCO DE PERSISTENCIA.           *
      *----------------------------------------------------------------*
       6400-ATUALIZAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-ATUALIZAR  TO TRUE
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE
           SET FRWKEL04-IND-BL-PERS    TO TRUE

           PERFORM 6500-EFETIVA-PERSISTENCIA-GAM
           .
      *----------------------------------------------------------------*
       6400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EFETIVAR A PERSISTENCIA NO GAM                  *
      *----------------------------------------------------------------*
       6500-EFETIVA-PERSISTENCIA-GAM   SECTION.
      *----------------------------------------------------------------*
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL

           MOVE DCOMWOLP-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWOLP-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           MOVE FRWKWAAA-SOLIC-MAIS-DADOS
                                       TO DCOMWOLP-INDICADOR-PAGINACAO

           MOVE DCOMWOLE-BLOCO-ENTRADA TO DCOMWOLP-BLOCO-ENTRADA

           MOVE DCOMWOLI-CHAVE-INI     TO DCOMWOLP-CHAVE-INI

           MOVE DCOMWOLI-CHAVE-FIM     TO DCOMWOLP-CHAVE-FIM

           MOVE DCOMW002-COD-RETORNO   TO DCOMWOLP-COD-RETORNO

           MOVE WRK-DCOMWOLP           TO FRWKEL04-BUFFER
                                         (1:FRWKEL04-TAM-BLOCO)

           PERFORM 5000-ACESSAR-GAM
           .
      *----------------------------------------------------------------*
       6500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSULTAR O BLOCO DE PERSISTENCIA.              *
      *----------------------------------------------------------------*
       6600-CONSULTAR-GAM-PERSISTENTE  SECTION.
      *----------------------------------------------------------------*

           SET  FRWKEL04-FUN-CONSULTAR TO TRUE

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE DCOMWOLP-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE DCOMWOLP-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-DCOMWOLP
           .
      *----------------------------------------------------------------*
       6600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA CONSISTENCIA DE CHAVES.                        *
      *----------------------------------------------------------------*
       6700-CONSISTIR-CHAVES           SECTION.
      *----------------------------------------------------------------*

           IF ((DCOMWOLP-P-INICIAL          OR DCOMWOLP-P-PRIMEIRA)
               AND FRWKWAAA-BLOCO-ANTERIOR)
               OR  (DCOMWOLP-P-ULTIMA       AND FRWKWAAA-BLOCO-SEGUINTE)
               OR  (DCOMWOLP-P-SEGUINTE     AND FRWKWAAA-BLOCO-SEGUINTE
               AND DCOMWOLP-COD-RETORNO     EQUAL ZEROS)
               OR  (DCOMWOLP-P-ANTERIOR     AND FRWKWAAA-BLOCO-ANTERIOR
               AND DCOMWOLP-COD-RETORNO     EQUAL ZEROS)
               MOVE 08                      TO FRWKGLAQ-COD-RETORNO
               MOVE '0190'                  TO FRWKGLAQ-COD-ERRO
               MOVE 'DCOM0016'              TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
       6700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ENVIA MENSAGEM GENERICA                                     *
      *----------------------------------------------------------------*
       9100-CRIAR-BLOCO-MENSAGEM       SECTION.
      *----------------------------------------------------------------*
           MOVE '9100-CRIAR-BLOCO-MENSAGEM'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE FRWKEL04-BLOCO-RETORNO
           MOVE SPACES                 TO FRWKEL04-BLOCO-VARIAVEL

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-CONSULTAR TO TRUE
           SET FRWKEL04-IND-BL-MENS    TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE 'FRWKGMEN'             TO FRWKEL04-ID-BLOCO
           MOVE LENGTH OF WRK-AREA-FRWKME
                                       TO FRWKEL04-TAM-BLOCO
                                          FRWKGMEN-TAM-LAYOUT
           MOVE WRK-AREA-FRWKME(1:FRWKEL04-TAM-BLOCO)
                                       TO FRWKEL04-BUFFER
           PERFORM 5000-ACESSAR-GAM
           .
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAJENTO DE ERRO LIVRE.                       *
      *----------------------------------------------------------------*
       9200-ERRO-LIVRE                 SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-LIVRE             TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-MENSAGEM           TO FRWKGLIV-COD-MENSAGEM
           MOVE WRK-IDIOMA             TO FRWKGLIV-IDIOMA
           MOVE WRK-PARAMETROS         TO FRWKGLIV-PARAMETROS
           MOVE WRK-COPY-LIVRE         TO WRK-BLOCO-INFO-ERRO

           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    TRATA DEBUG                                                 *
      *----------------------------------------------------------------*
       9250-ERRO-DEBUG                 SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-LIVRE             TO TRUE
           MOVE 1                      TO FRWKGLIV-IDIOMA
           MOVE 'DCOM0035'             TO FRWKGLIV-COD-MENSAGEM
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE WRK-PARAMETROS         TO FRWKGLIV-PARAMETROS
           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-COPY-LIVRE         TO WRK-BLOCO-INFO-ERRO

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF

           MOVE SPACES                TO WRK-PARAMETROS

           .
      *----------------------------------------------------------------*
       9250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-MODULO            TO TRUE
           MOVE 16                     TO FRWKGLAQ-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO

           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-CICS              TO TRUE
           MOVE 16                     TO FRWKGLAQ-COD-RETORNO
           MOVE 'DCOM0010'             TO FRWKGLAQ-COD-MENSAGEM
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO

           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
                LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF

           PERFORM 3000-FINALIZAR
           .
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
