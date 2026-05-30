      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3ALL.
       AUTHOR.     LUCIANDRA SILVEIRA.
      *================================================================*
      *                         WIPRO DO BRASIL                        *
      *================================================================*
      *    PROGRAMA...: DCOM3ALL                                       *
      *    PROGRAMADOR: LUCIANDRA SILVEIRA                             *
      *    ANALISTA...: RICARDO JAMMAL                                 *
      *    DATA.......: 07/08/2023                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO...: INCLUSAO E MANUTENCAO DE TESTEMUNHAS DA AGENCIA*
      *    PROJETO....: RTN 2019-0456637-5-341 DCOM TF WEB             *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CALEWAAA - BOOK DE INTERFACE DO FUNCIONAL CALE1000 -        *
      *               OBTER DATA ATUAL                                 *
      *    DCOMW002 - AREA DE COMUNICACAO DE MENSAGENS DE PROCESSAMENTO*
      *    DCOMWA1I - AREA DE INTERFACE DO MODULO DCOM3ALL.            *
      *    DCOMWD4E - AREA DE ENTRADA DO MODULO DE ACESSO DCOM4D4I     *
      *                                                   DCOM4D4U     *
      *                                                   DCOM4D4S.    *
      *    DCOMWD4S - AREA DE ENTRADA DO MODULO DE ACESSO DCOM4D4I     *
      *                                                   DCOM4D4U     *
      *                                                   DCOM4D4S.    *
      *    DCOMWK9E - AREA DE ENTRADA DO MODULO DE ACESSO DCOM4K9F     *
      *                                                   DCOM4K9S.    *
      *    DCOMWK9S - AREA DE SAIDA   DO MODULO DE ACESSO DCOM4K9F     *
      *                                                   DCOM4K9S.    *
      *    DCOMWNSI - AREA DE COMUNICACAO MODULO DCOM3NSI              *
      *    DCOMWLOG - AREA DE COMUNICACAO MODULO DCOM3LOG              *
      *    I#DCOMHX - AREA AUXILIAR PARA HEXAVISION.                   *
      *----------------------------------------------------------------*
      *    BOOK'S ARQUITETURAIS:                                       *
      *    I#FRWKCI - AREA DE TRATAMENTO DE LOG (ERRO CICS  ).         *
      *    I#FRWKLI - AREA DE TRATAMENTO DE LOG (ERRO LIVRE).          *
      *    I#FRWKMD - AREA DE TRATAMENTO DE LOG (ERRO MODULO).         *
      *    I#FRWKGE - AREA DE INFORMACAO        (LOG DE ERRO).         *
      *----------------------------------------------------------------*
      *    MODULOS....:                                                *
      *    CALE1000 - OBTER DATAS                                      *
      *    DCOM3NSI - MODULO PARA OBTER NUMERO DE CONTROLE SEQUENCIAL  *
      *               DO SISTEMA DCOM.                                 *
      *    DCOM3LOG - MODULO PARA INCLUSAO DE LOG DO SISTEMA DCOM      *
      *    DCOM4D4U - MODULO BASICO DE ACESSO A TABELA DCOMB0D4- UPDATE*
      *    DCOM4K9F - MODULO BASICO DE ACESSO A TABELA DCOMB0K9- FETCH *
      *    DCOM4K9S - MODULO BASICO DE ACESSO A TABELA DCOMB0K9- SELECT*
      *    DCOM4K9I - MODULO BASICO DE ACESSO A TABELA DCOMB0K9- INSERT*
      *    FRWK1999 - TRATAMENTO DE ERROS - BOOK I#FRWKGE.             *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3ALL - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-PROGRAMA                PIC  X(008)         VALUE
           'DCOM3ALL'.
       77  WRK-FRWK1999                PIC  X(008)         VALUE
           'FRWK1999'.
       77  WRK-CALE1000                PIC  X(008)         VALUE
           'CALE1000'.

       01  WRK-AUXILIARES.
           05 IND-1                    PIC  9(003) COMP-3  VALUE ZEROS.
           05 IND-2                    PIC  9(003) COMP-3  VALUE ZEROS.
           05 WRK-FIM                  PIC  X(001)         VALUE SPACES.
           05 WRK-IND-PAG              PIC  X(001)         VALUE SPACES.
           05 WRK-MODULO               PIC  X(008)         VALUE SPACES.
           05 WRK-TEM-CADASTRO         PIC  X(001)         VALUE SPACES.
           05 WRK-PROX-COD-PESSOA      PIC  9(009)         VALUE ZEROS.
           05 WRK-DATUAL-DB2           PIC  X(010)         VALUE SPACES.
           05 WRK-TIMESTAMP-ATUAL      PIC  X(026)         VALUE SPACES.

           05 WRK-DATA-ATU-AMD.
              10 WRK-ANO-AMD           PIC  9(004)         VALUE ZEROS.
              10 WRK-MES-AMD           PIC  9(002)         VALUE ZEROS.
              10 WRK-DIA-AMD           PIC  9(002)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-DATA-ATU-AMD.
              10 WRK-DATA-ATU-AMD-N    PIC  9(008).

       01  WRK-AREA-DCOMB0D4.
           05 WRK-D4-NPSSOA-DESC-COML  PIC  9(009)         VALUE ZEROS.
4S2511*    05 WRK-D4-CCNPJ-CPF         PIC  9(009)         VALUE ZEROS.
4S2511     05 WRK-D4-CCNPJ-CPF         PIC  X(09)          VALUE SPACES.
4S2511*    05 WRK-D4-CFLIAL-CNPJ       PIC  9(005)         VALUE ZEROS.
4S2511     05 WRK-D4-CFLIAL-CNPJ       PIC  X(04)          VALUE SPACES.
           05 WRK-D4-CCTRL-CNPJ-CPF    PIC  9(002)         VALUE ZEROS.
           05 WRK-D4-CDOCTO-ID         PIC  X(015)         VALUE SPACES.
           05 WRK-D4-IPSSOA-DESC-COML  PIC  X(060)         VALUE SPACES.
           05 WRK-D4-CCLUB             PIC  9(010)         VALUE ZEROS.
           05 WRK-D4-DINCL-REG         PIC  X(010)         VALUE SPACES.
           05 WRK-D4-HULT-ATULZ        PIC  X(026)         VALUE SPACES.
           05 WRK-D4-CFUNC-BDSCO       PIC  9(009)         VALUE ZEROS.
           05 WRK-D4-CTERM             PIC  X(008)         VALUE SPACES.
           05 WRK-D4-CINDCD-TPO-PSSOA  PIC  X(001)         VALUE SPACES.

       01  WRK-AREA-DCOMB0K9.
           10 WRK-K9-CBCO              PIC  9(003)         VALUE ZEROS.
           10 WRK-K9-CAG-BCRIA         PIC  9(005)         VALUE ZEROS.
           10 WRK-K9-NPSSOA-DESC-COML  PIC  9(009)         VALUE ZEROS.
           10 WRK-K9-DULT-UTILZ        PIC  X(010)         VALUE SPACES.
           10 WRK-K9-HULT-ATULZ        PIC  X(026)         VALUE SPACES.
           10 WRK-K9-DINCL-REG         PIC  X(010)         VALUE SPACES.
           10 WRK-K9-CFUNC-BDSCO       PIC  9(009)         VALUE ZEROS.
           10 WRK-K9-CTERM             PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA CALE1000 - DATA ATUAL ***'.
      *----------------------------------------------------------------*

       COPY CALEWAAA.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILIAR PARA HEXAVISION ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMHX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM3LOG ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM3LOG                PIC  X(008)         VALUE
           'DCOM3LOG'.

       01  WRK-AREA-DCOM3LOG.
           COPY DCOMW002.
           COPY DCOMWLOG.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM3NSI ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM3NSI                PIC  X(008)         VALUE
           'DCOM3NSI'.

       01  WRK-AREA-DCOM3NSI.
           COPY DCOMW002.
           COPY DCOMWNSI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4D4I ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4D4I                PIC  X(008)         VALUE
           'DCOM4D4I'.

       01  WRK-AREA-DCOM4D4I.
           COPY 'DCOMW002'.
           COPY 'DCOMWD4E'.
           COPY 'DCOMWD4S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4D4S ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4D4S                PIC  X(008)         VALUE
           'DCOM4D4S'.

       01  WRK-AREA-DCOM4D4S.
           COPY 'DCOMW002'.
           COPY 'DCOMWD4E'.
           COPY 'DCOMWD4S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4D4U ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4D4U                PIC  X(008)         VALUE
           'DCOM4D4U'.

       01  WRK-AREA-DCOM4D4U.
           COPY 'DCOMW002'.
           COPY 'DCOMWD4E'.
           COPY 'DCOMWD4S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4K9D ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4K9D                PIC  X(008)         VALUE
           'DCOM4K9D'.

       01  WRK-AREA-DCOM4K9D.
           COPY 'DCOMW002'.
           COPY 'DCOMWK9E'.
           COPY 'DCOMWK9S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4K9F ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4K9F                PIC  X(008)         VALUE
           'DCOM4K9F'.

       01  WRK-AREA-DCOM4K9F.
           COPY 'DCOMW002'.
           COPY 'DCOMWK9E'.
           COPY 'DCOMWK9S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4K9I ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4K9I                PIC  X(008)         VALUE
           'DCOM4K9I'.

       01  WRK-AREA-DCOM4K9I.
           COPY 'DCOMW002'.
           COPY 'DCOMWK9E'.
           COPY 'DCOMWK9S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4K9S ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM4K9S                PIC  X(008)         VALUE
           'DCOM4K9S'.

       01  WRK-AREA-DCOM4K9S.
           COPY 'DCOMW002'.
           COPY 'DCOMWK9E'.
           COPY 'DCOMWK9S'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRATAMENTO DE ERROS CICS ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRATAMENTO DE ERRO LIVRE ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-LIVRE.
           COPY 'I#FRWKLI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TRATAMENTO DE ERROS MODULO ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA FRAMEWORK ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERR0    PIC  X(001)
                                       OCCURS  0 TO 00526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3ALL - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           COPY DCOMW002.
           COPY DCOMWA1I.

      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA DE MACRO PROCESSAMENTO DO PROGRAMA.                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA PROCEDIMENTOS INICIAIS DE EXECUCAO.                *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE WRK-AUXILIARES
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO
                      FRWKGERR-REGISTRO
                      FRWKGHEA-REGISTRO
                      DCOMWA1I-BLOCO-SAIDA
                      DCOMWK9E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4K9F
                      DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                      DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA

           PERFORM 1100-CONSISTIR-ENTRADA.

           PERFORM 1200-OBTER-DATA-ATUAL.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTIR OS DADOS DE ENTRADA.                     *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-CONSISTIR-ENTRADA'    TO FRWKGHEA-IDEN-PARAGRAFO.

           IF  DCOMWA1I-E-CBCO         NOT NUMERIC OR
               DCOMWA1I-E-CBCO         EQUAL ZEROS
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0010'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0037'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'CODIGO DO BANCO INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWA1I-E-CAG-BCRIA    NOT NUMERIC OR
               DCOMWA1I-E-CAG-BCRIA    EQUAL ZEROS
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0020'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0003'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'CODIGO DE AGENCIA INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE  DCOMWA1I-E-FUNCAO
      **-->    01 - LISTAR TESTEMUNHAS
      **-->    --------------------------------------
               WHEN  01
                     CONTINUE

      **-->    02 - CONSULTAR TESTEMUNHA
      **-->    --------------------------------------
               WHEN  02
4S2511*          IF (DCOMWA1I-E-CCPF      NOT NUMERIC  OR
4S2511*              DCOMWA1I-E-CCPF      EQUAL ZEROS) OR
4S2511*              DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
4S2511           IF DCOMWA1I-E-CCPF      EQUAL SPACES OR
4S2511              DCOMWA1I-E-CCPF      EQUAL LOW-VALUES OR
                    DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0030'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CPF DA TESTEMUNHA INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

      **-->    03 - INCLUIR TESTEMUNHA
      **-->    --------------------------------------
               WHEN  03
4S2511*            IF (DCOMWA1I-E-CCPF      NOT NUMERIC  OR
4S2511*                DCOMWA1I-E-CCPF      EQUAL ZEROS) OR
4S2511*                DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
4S2511             IF DCOMWA1I-E-CCPF      EQUAL SPACES OR
4S2511                DCOMWA1I-E-CCPF      EQUAL LOW-VALUES OR
                       DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0040'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CPF DA TESTEMUNHA INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF
                 IF  DCOMWA1I-E-NPSSOA-DESC-COML NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0050'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'NUMERO IDENTIFICADOR DA PESSOA TESTEMUNHA INV
      -                   'ALIDO'      TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF
                 IF  DCOMWA1I-E-IPSSOA-DESC-COML
                                       EQUAL SPACES OR LOW-VALUES
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0060'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'NOME DA TESTEMUNHA NAO INFORMADO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

                 IF  DCOMWA1I-E-CCLUB  NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0070'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0060'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CLUB INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

      **-->    04 - ALTERAR TESTEMUNHA
      **-->    --------------------------------------
               WHEN  04
4S2511*            IF (DCOMWA1I-E-CCPF      NOT NUMERIC  OR
4S2511*                DCOMWA1I-E-CCPF      EQUAL ZEROS) OR
4S2511*                DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
4S2511             IF DCOMWA1I-E-CCPF      EQUAL SPACES OR
4S2511                DCOMWA1I-E-CCPF      EQUAL LOW-VALUES OR
                       DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0080'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CPF DA TESTEMUNHA INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF
                 IF  DCOMWA1I-E-IPSSOA-DESC-COML
                                       EQUAL SPACES OR LOW-VALUES
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0090'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'NOME DA TESTEMUNHA NAO INFORMADO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

                 IF  DCOMWA1I-E-CCLUB  NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0100'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0060'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CLUB INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

      **-->    05 - EXCLUIR TESTEMUNHA
      **-->    -------------------------------------------
               WHEN  05
4S2511*            IF (DCOMWA1I-E-CCPF      NOT NUMERIC  OR
4S2511*                DCOMWA1I-E-CCPF      EQUAL ZEROS) OR
4S2511*                DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
4S2511             IF DCOMWA1I-E-CCPF      EQUAL SPACES OR
4S2511                DCOMWA1I-E-CCPF      EQUAL LOW-VALUES OR
                       DCOMWA1I-E-CCTRL-CPF NOT NUMERIC
                     MOVE 08           TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                     MOVE '0110'       TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                     MOVE 'DCOM0035'   TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                     MOVE 'CPF DA TESTEMUNHA INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                     PERFORM 3000-FINALIZAR
                 END-IF

               WHEN OTHER
                    MOVE 08            TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                    MOVE '0120'        TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                    MOVE 'DCOM0040'    TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                    MOVE 'CODIGO DA FUNCAO INVALIDO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                    PERFORM 9500-TRATAR-ERRO-LIVRE
           END-EVALUATE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*

           MOVE '1200-OBTER-DATA-ATUAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE CALEWAAA-BLOCO-RETORNO
                      CALEWAAA-SAIDA

           EXEC CICS LINK
                PROGRAM  (WRK-CALE1000)
                COMMAREA (CALEWAAA)
                LENGTH   (LENGTH OF CALEWAAA)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE '0130'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           IF  CALEWAAA-COD-RET        NOT EQUAL ZEROS
               MOVE 16                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0140'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0063'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE WRK-CALE1000       TO WRK-MODULO
               MOVE CALEWAAA-BLOCO-RETORNO
                                       TO DCOMW002-BLOCO-RETORNO
                                       OF DFHCOMMAREA
               MOVE 'ERRO AO OBTER OU CALCULAR DATA NO MODULO CALE'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           MOVE CALEWAAA-DT-GREGO-AAAAMMDD   TO WRK-DATA-ATU-AMD-N.
           MOVE CALEWAAA-DT-GREGO-P-DDMMAAAA TO WRK-DATUAL-DB2.
           MOVE CALEWAAA-HH-TIMESTAMP-DB2    TO WRK-TIMESTAMP-ATUAL.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DO PROCESSAMENTO PRINCIPAL DO PROGRAMA.                 *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE DCOMWA1I-E-FUNCAO
               WHEN 01
      **-->         01 - LISTAR TESTEMUNHAS: AGENCIA/CONTA
                    MOVE 'I'           TO DCOMWK9E-INDICADOR-PAGINACAO
                                       OF WRK-AREA-DCOM4K9F

                    PERFORM 2100-LISTAR-TESTEMUNHAS
                      UNTIL IND-2      EQUAL 100
                         OR WRK-FIM    EQUAL 'S'

               WHEN 02
      **-->         02 - CONSULTAR TESTEMUNHA
                    PERFORM 2200-CONSULTAR-TESTEMUNHAS

               WHEN 03
      **-->         03 - INCLUIR TESTEMUNHA
                    PERFORM 2300-INCLUIR-TESTEMUNHA

               WHEN 04
      **-->         04 - ALTERAR TESTEMUNHA
                    PERFORM 2400-ALTERAR-TESTEMUNHA

               WHEN 05
      **-->         05 - EXCLUIR TESTEMUNHA
                    PERFORM 2500-EXCLUIR-TESTEMUNHA
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ACESSO AO MODULO DCOM4K9F PARA LISTAR TESTEMUNHAS DA AGENCIA   *
      *----------------------------------------------------------------*
       2100-LISTAR-TESTEMUNHAS         SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LISTAR-TESTEMUNHAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWK9E-ENTRADA          OF WRK-AREA-DCOM4K9F
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9F
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9F.

      **-> FORMATACAO DE ARGUMENTOS DE PESQUISA
      *    ------------------------------------

           EVALUATE DCOMWA1I-E-FUNCAO
      **->     01 - LISTAR POR CLIENTE: AGENCIA/CONTA
      **->     --------------------------------------
               WHEN 01
                    MOVE DCOMWA1I-E-CBCO
                                       TO DCOMWK9E-CBCO
                                       OF WRK-AREA-DCOM4K9F
                    MOVE DCOMWA1I-E-CAG-BCRIA
                                       TO DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9F
                    MOVE 'PARCIAL-02'  TO DCOMWK9E-INSTRUCAO
                                       OF WRK-AREA-DCOM4K9F
           END-EVALUATE.

      *--> FORMATACAO DE CAMPOS DE PAGINACAO
           MOVE 50                     TO DCOMWK9E-MAX-OCORR
                                       OF WRK-AREA-DCOM4K9F

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4K9F)
                COMMAREA (WRK-AREA-DCOM4K9F)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4K9F)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0150'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           IF  DCOMW002-COD-RETORNO    OF WRK-AREA-DCOM4K9F
                                       EQUAL ZEROS OR 01
               PERFORM 2110-RELACIONAR-TESTEMUNHAS

               IF WRK-FIM              NOT EQUAL 'S'
                  IF DCOMW002-COD-RETORNO
                                       OF WRK-AREA-DCOM4K9F EQUAL 01
                     MOVE 'S'          TO DCOMWK9E-INDICADOR-PAGINACAO
                                       OF WRK-AREA-DCOM4K9F
                     GO TO 2100-LISTAR-TESTEMUNHAS
                  ELSE
                     MOVE 'S'          TO WRK-FIM
                  END-IF
               END-IF
           ELSE
               MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9F
                 TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
               MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9F
                 TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
               MOVE WRK-DCOM4K9F              TO WRK-MODULO
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> CONSULTA EFETUADA COM SUCESSO
           MOVE 'DCOM0001'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TRATAR LISTA DE TESTEMUNHAS DA AGENCIA                         *
      *----------------------------------------------------------------*
       2110-RELACIONAR-TESTEMUNHAS     SECTION.
      *----------------------------------------------------------------*

           MOVE '2110-RELACIONAR-TESTEMUNHAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2120-DETALHAR-TESTEMUNHAS
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER DCOMWK9S-QTDE-RETORNADA
                                            OF WRK-AREA-DCOM4K9F.

           IF  IND-2                   EQUAL 100
               MOVE 'S'                TO WRK-FIM
           END-IF.

           MOVE IND-2                  TO DCOMWA1I-QTDE-TESTEMUNHAS.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA DETALHAR DADOS DE TESTEMUNHAS DA AGENCIA           *
      *----------------------------------------------------------------*
       2120-DETALHAR-TESTEMUNHAS       SECTION.
      *----------------------------------------------------------------*

           MOVE '2120-DETALHAR-TESTEMUNHAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           ADD 1                       TO IND-2.

           MOVE DCOMWK9S-CBCO          OF WRK-AREA-DCOM4K9F   (IND-1)
                                       TO DCOMWA1I-S-CBCO     (IND-2).
           MOVE DCOMWK9S-CAG-BCRIA     OF WRK-AREA-DCOM4K9F   (IND-1)
                                       TO DCOMWA1I-S-CAG-BCRIA(IND-2).
           MOVE DCOMWK9S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9F   (IND-1)
                                       TO DCOMWA1I-S-NPSSOA-DESC-COML
                                                              (IND-2).

           PERFORM 2121-OBTER-DADOS-PESSOA.

           MOVE DCOMWD4S-CCNPJ-CPF     OF WRK-AREA-DCOM4D4S   (1)
                                       TO DCOMWA1I-S-CCPF     (IND-2).
           MOVE DCOMWD4S-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S   (1)
                                       TO DCOMWA1I-S-CCTRL-CPF(IND-2).
           MOVE DCOMWD4S-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S   (1)
                                       TO DCOMWA1I-S-IPSSOA-DESC-COML
                                                              (IND-2).
           MOVE DCOMWD4S-CCLUB         OF WRK-AREA-DCOM4D4S   (1)
                                       TO DCOMWA1I-S-CCLUB    (IND-2).
           MOVE DCOMWD4S-CDOCTO-ID     OF WRK-AREA-DCOM4D4S   (1)
                                       TO DCOMWA1I-S-CDOCTO-ID(IND-2).
           MOVE 'S'                    TO DCOMWA1I-S-TEM-NA-AGENCIA
                                                              (IND-2).

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DADOS DA PESSOA TESTEMUNHA                               *
      *----------------------------------------------------------------*
       2121-OBTER-DADOS-PESSOA         SECTION.
      *----------------------------------------------------------------*

           MOVE '2121-OBTER-DADOS-PESSOA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWD4E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4D4S
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4S
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4S.

           MOVE DCOMWA1I-S-NPSSOA-DESC-COML(IND-1)
                                       TO DCOMWD4E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S.
           MOVE 'TOTAL'                TO DCOMWD4E-INSTRUCAO
                                       OF WRK-AREA-DCOM4D4S.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4D4S)
                COMMAREA (WRK-AREA-DCOM4D4S)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4D4S)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0160'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4D4S
               WHEN ZEROS
                    CONTINUE

               WHEN 03
                    MOVE 08            TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                    MOVE '0170'        TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                    MOVE 'DCOM0035'    TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                    STRING 'DADOS DA PESSOA TESTEMUNHA INEXISTENTE( '
                            DCOMWA1I-S-NPSSOA-DESC-COML(IND-1) ' )'
                    DELIMITED BY SIZE INTO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                    PERFORM 3000-FINALIZAR

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4S
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4S
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4D4S              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DADOS DA TESTEMUNHA                                      *
      *----------------------------------------------------------------*
       2200-CONSULTAR-TESTEMUNHAS      SECTION.
      *----------------------------------------------------------------*

           MOVE '2200-CONSULTAR-TESTEMUNHAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2210-CONSULTAR-NA-AGENCIA.

           IF  DCOMWA1I-S-TEM-NA-AGENCIA
                                   (1) EQUAL 'S'
               MOVE DCOMWK9S-CBCO      OF WRK-AREA-DCOM4K9S   (1)
                                       TO DCOMWA1I-S-CBCO     (1)
               MOVE DCOMWK9S-CAG-BCRIA OF WRK-AREA-DCOM4K9S   (1)
                                       TO DCOMWA1I-S-CAG-BCRIA(1)
               MOVE DCOMWK9S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9S   (1)
                                       TO DCOMWA1I-S-NPSSOA-DESC-COML(1)

               MOVE 1                  TO IND-1
               PERFORM 2121-OBTER-DADOS-PESSOA
           ELSE
               PERFORM 2220-OBTER-DADOS-PESSOA-CPF

               IF  WRK-TEM-CADASTRO    EQUAL 'N'
                   MOVE 08             TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                   MOVE '0180'         TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                   MOVE 'DCOM0035'     TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                   STRING 'DADOS DA PESSOA TESTEMUNHA INEXISTENTE( '
                           DCOMWA1I-E-CCPF '-'
                           DCOMWA1I-E-CCTRL-CPF ' )'
                   DELIMITED BY SIZE INTO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           MOVE 1                      TO DCOMWA1I-QTDE-TESTEMUNHAS.
           MOVE DCOMWD4S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-NPSSOA-DESC-COML(1)
           MOVE DCOMWD4S-CCNPJ-CPF     OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-CCPF            (1)
           MOVE DCOMWD4S-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-CCTRL-CPF       (1)
           MOVE DCOMWD4S-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-IPSSOA-DESC-COML(1)
           MOVE DCOMWD4S-CCLUB         OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-CCLUB           (1)
           MOVE DCOMWD4S-CDOCTO-ID     OF WRK-AREA-DCOM4D4S          (1)
                                       TO DCOMWA1I-S-CDOCTO-ID       (1)

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> CONSULTA EFETUADA COM SUCESSO
           MOVE 'DCOM0001'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSULTAR SE TESTEMUNHA ESTA CADASTRADO NA AGENCIA             *
      *----------------------------------------------------------------*
       2210-CONSULTAR-NA-AGENCIA       SECTION.
      *----------------------------------------------------------------*

           MOVE '2210-CONSULTAR-NA-AGENCIA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWK9E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4K9S
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9S
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9S.

           MOVE DCOMWA1I-E-CBCO        TO DCOMWK9E-CBCO
                                       OF WRK-AREA-DCOM4K9S
           MOVE DCOMWA1I-E-CAG-BCRIA   TO DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9S

           MOVE DCOMWA1I-E-CCPF        TO DCOMWK9E-V-CCNPJ-CPF
                                       OF WRK-AREA-DCOM4K9S
           MOVE DCOMWA1I-E-CCTRL-CPF   TO DCOMWK9E-V-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4K9S
           MOVE 'PARCIAL-01'           TO DCOMWK9E-INSTRUCAO
                                       OF WRK-AREA-DCOM4K9S

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4K9S)
                COMMAREA (WRK-AREA-DCOM4K9S)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4K9S)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                   NOT EQUAL DFHRESP(NORMAL))
               MOVE '0190'               TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4K9S
               WHEN ZEROS
                    MOVE 'S'
                      TO DCOMWA1I-S-TEM-NA-AGENCIA  (1)

               WHEN 03
                    MOVE 'N'
                      TO DCOMWA1I-S-TEM-NA-AGENCIA  (1)

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9S
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9S
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4K9S              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DADOS DA PESSOA TESTEMUNHA A PARTIR DO CPF               *
      *----------------------------------------------------------------*
       2220-OBTER-DADOS-PESSOA-CPF     SECTION.
      *----------------------------------------------------------------*

           MOVE '2220-OBTER-DADOS-PESSOA-CPF'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWD4E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4D4S
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4S
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4S.

           MOVE DCOMWA1I-E-CCPF        TO DCOMWD4E-CCNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S.
           MOVE ZEROS                  TO DCOMWD4E-CFLIAL-CNPJ
                                       OF WRK-AREA-DCOM4D4S.
           MOVE DCOMWA1I-E-CCTRL-CPF   TO DCOMWD4E-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S.
           MOVE 'PARCIAL-01'           TO DCOMWD4E-INSTRUCAO
                                       OF WRK-AREA-DCOM4D4S.
           MOVE SPACES                 TO WRK-TEM-CADASTRO.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4D4S)
                COMMAREA (WRK-AREA-DCOM4D4S)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4D4S)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0200'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4D4S
               WHEN ZEROS
                    MOVE 'S'           TO WRK-TEM-CADASTRO

               WHEN 03
                    MOVE 'N'           TO WRK-TEM-CADASTRO

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4S
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4S
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4D4S              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INCLUIR TESTEMUNHA PARA A AGENCIA                              *
      *----------------------------------------------------------------*
       2300-INCLUIR-TESTEMUNHA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2220-OBTER-DADOS-PESSOA-CPF.

           IF  WRK-TEM-CADASTRO        EQUAL 'N'
               PERFORM 2310-OBTER-NRO-PESSOA
               PERFORM 2320-INCLUIR-PESSOA
           END-IF.

           PERFORM 2330-INCLUIR-TESTEMUNHA-AG.

           PERFORM 2340-TRATAR-LOG.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER PROXIMO NUMERO DA PESSOA                                 *
      *----------------------------------------------------------------*
       2310-OBTER-NRO-PESSOA           SECTION.
      *----------------------------------------------------------------*

           MOVE '2310-OBTER-NRO-PESSOA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 029                    TO DCOMWNSI-E-CELMTO-DESC-COML
           MOVE 001                    TO DCOMWNSI-E-WCHAVE-PRINC-ELMTO
           MOVE 009                    TO DCOMWNSI-E-QDIG-SEQ-ELMTO

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3NSI)
                COMMAREA (WRK-AREA-DCOM3NSI)
                LENGTH   (LENGTH OF WRK-AREA-DCOM3NSI)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0210'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO  OF WRK-AREA-DCOM3NSI
               WHEN ZEROS
                    MOVE DCOMWNSI-S-NULT-SEQ-ELMTO
                                          OF WRK-AREA-DCOM3NSI
                                          TO WRK-PROX-COD-PESSOA

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM3NSI
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM3NSI
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM3NSI              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INCLUIR PESSOA NA BASE DE DADOS DO DCOM                        *
      *----------------------------------------------------------------*
       2320-INCLUIR-PESSOA             SECTION.
      *----------------------------------------------------------------*

           MOVE '2320-INCLUIR-PESSOA'  TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWD4E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4D4I
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4I
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4I.

           MOVE WRK-PROX-COD-PESSOA    TO DCOMWD4E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-E-CCPF        TO DCOMWD4E-CCNPJ-CPF
                                       OF WRK-AREA-DCOM4D4I.
           MOVE ZEROS                  TO DCOMWD4E-CFLIAL-CNPJ
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-E-CCTRL-CPF   TO DCOMWD4E-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-E-CDOCTO-ID   TO DCOMWD4E-CDOCTO-ID
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-E-IPSSOA-DESC-COML
                                       TO DCOMWD4E-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-E-CCLUB       TO DCOMWD4E-CCLUB
                                       OF WRK-AREA-DCOM4D4I.
           MOVE WRK-DATUAL-DB2         TO DCOMWD4E-DINCL-REG
                                       OF WRK-AREA-DCOM4D4I.
           MOVE WRK-TIMESTAMP-ATUAL    TO DCOMWD4E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-CTERM         TO DCOMWD4E-CTERM
                                       OF WRK-AREA-DCOM4D4I.
           MOVE DCOMWA1I-CAUTEN-SEGRC-N
                                       TO DCOMWD4E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4D4I.
           MOVE 'F'                    TO DCOMWD4E-CINDCD-TPO-PSSOA
                                       OF WRK-AREA-DCOM4D4I.
           MOVE 'TOTAL'                TO DCOMWD4E-INSTRUCAO
                                       OF WRK-AREA-DCOM4D4I.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4D4I)
                COMMAREA (WRK-AREA-DCOM4D4I)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4D4I)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0220'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4D4I
               WHEN ZEROS
                    CONTINUE

               WHEN 04
                    CONTINUE

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4I
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4I
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4D4I              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * INCLUIR TESTEMUNHA PARA A AGENCIA                              *
      *----------------------------------------------------------------*
       2330-INCLUIR-TESTEMUNHA-AG      SECTION.
      *----------------------------------------------------------------*

           MOVE '2330-INCLUIR-TESTEMUNHA-AG'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWK9E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4K9I
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9I
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9I.

           MOVE DCOMWA1I-E-CBCO        TO DCOMWK9E-CBCO
                                       OF WRK-AREA-DCOM4K9I.
           MOVE DCOMWA1I-E-CAG-BCRIA   TO DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9I.

           IF WRK-TEM-CADASTRO         EQUAL 'N'
              MOVE WRK-PROX-COD-PESSOA TO DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9I
           ELSE
              MOVE DCOMWD4S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9I
           END-IF.

           MOVE '01.01.0001'           TO DCOMWK9E-DULT-UTILZ
                                       OF WRK-AREA-DCOM4K9I.
           MOVE DCOMWA1I-CAUTEN-SEGRC-N
                                       TO DCOMWK9E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4K9I.
           MOVE DCOMWA1I-CTERM         TO DCOMWK9E-CTERM
                                       OF WRK-AREA-DCOM4K9I.
           MOVE WRK-DATUAL-DB2         TO DCOMWK9E-DINCL-REG
                                       OF WRK-AREA-DCOM4K9I.
           MOVE WRK-TIMESTAMP-ATUAL    TO DCOMWK9E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4K9I.
           MOVE 'TOTAL'                TO DCOMWK9E-INSTRUCAO
                                       OF WRK-AREA-DCOM4K9I

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4K9I)
                COMMAREA (WRK-AREA-DCOM4K9I)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4K9I)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                   NOT EQUAL DFHRESP(NORMAL))
               MOVE '0230'               TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4K9I
               WHEN ZEROS
                    CONTINUE

               WHEN 04
                    MOVE 08              TO DCOMW002-COD-RETORNO
                                         OF DFHCOMMAREA
                    MOVE '0240'          TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
                    MOVE 'DCOM0035'      TO DCOMW002-COD-MENSAGEM
                                         OF DFHCOMMAREA
                    STRING 'TESTEMUNHA JA CADASTRADA NA AGENCIA (CPF: '
                           DCOMWA1I-E-CCPF '-'
                           DCOMWA1I-E-CCTRL-CPF ' - AGENCIA: '
                           DCOMWA1I-E-CAG-BCRIA ')'
                    DELIMITED BY SIZE    INTO DCOMW002-PARAMETRO
                                         OF DFHCOMMAREA
                    PERFORM 3000-FINALIZAR

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9I
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9I
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4K9I              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> INCLUSAO EFETUADA COM SUCESSO
           MOVE 'DCOM0045'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * TRATAR INCLUSAO DE LOG                                         *
      *----------------------------------------------------------------*
       2340-TRATAR-LOG                 SECTION.
      *----------------------------------------------------------------*

           MOVE '2340-TRATAR-LOG'      TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWLOG-BLOCO-ENTRADA
                      DCOMWLOG-BLOCO-SAIDA
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM3LOG.

           INITIALIZE WRK-AREA-DCOMB0D4
                      WRK-AREA-DCOMB0K9.

      **-> 029 = ELEMENTO "PESSOA"
           MOVE 029                    TO DCOMWLOG-E-CELMTO-DESC-COML

      **-> 084 = EVENTO "TESTEMUNHA"
           MOVE 084                    TO DCOMWLOG-E-CEVNTO-LOG-DESC

           EVALUATE DCOMWA1I-E-FUNCAO
               WHEN 03
      **-->         03 - INCLUIR TESTEMUNHA
                    MOVE 'I'           TO DCOMWLOG-E-CINDCD-MANUT-REG
                    MOVE DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9I
                                       TO HX-NUM-09-CS

                    STRING DCOMWA1I-E-CBCO
                           DCOMWA1I-E-CAG-BCRIA
                           HX-NUM-09-SS
                    DELIMITED BY SIZE  INTO DCOMWLOG-E-CCHAVE-ELMTO-DESC

                    MOVE ZEROS         TO DCOMWLOG-E-ANTES-LEN
                    MOVE SPACES        TO DCOMWLOG-E-ANTES-TEXT

                    MOVE DCOMWK9E-CBCO OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-CBCO
                    MOVE DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-CAG-BCRIA
                    MOVE DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-NPSSOA-DESC-COML
                    MOVE DCOMWK9E-DULT-UTILZ
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-DULT-UTILZ
                    MOVE DCOMWK9E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-HULT-ATULZ
                    MOVE DCOMWK9E-DINCL-REG
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-DINCL-REG
                    MOVE DCOMWK9E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-CFUNC-BDSCO
                    MOVE DCOMWK9E-CTERM
                                       OF WRK-AREA-DCOM4K9I
                                       TO WRK-K9-CTERM

                    MOVE LENGTH WRK-AREA-DCOMB0K9
                                       TO DCOMWLOG-E-APOS-LEN
                    MOVE WRK-AREA-DCOMB0K9
                                       TO DCOMWLOG-E-APOS-TEXT

               WHEN 04
      **-->         04 - ALTERAR TESTEMUNHA
                    MOVE 'A'           TO DCOMWLOG-E-CINDCD-MANUT-REG
                    MOVE DCOMWD4E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4U
                                       TO HX-NUM-09-CS
                    STRING DCOMWA1I-E-CBCO
                           DCOMWA1I-E-CAG-BCRIA
                           HX-NUM-09-SS
                    DELIMITED BY SIZE  INTO DCOMWLOG-E-CCHAVE-ELMTO-DESC

                    MOVE DCOMWD4S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-NPSSOA-DESC-COML
                    MOVE DCOMWD4S-CCNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CCNPJ-CPF
                    MOVE DCOMWD4S-CFLIAL-CNPJ
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CFLIAL-CNPJ
                    MOVE DCOMWD4S-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CCTRL-CNPJ-CPF
                    MOVE DCOMWD4S-CDOCTO-ID
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CDOCTO-ID
                    MOVE DCOMWD4S-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-IPSSOA-DESC-COML
                    MOVE DCOMWD4S-CCLUB
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CCLUB
                    MOVE DCOMWD4S-DINCL-REG
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-DINCL-REG
                    MOVE DCOMWD4S-HULT-ATULZ
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-HULT-ATULZ
                    MOVE DCOMWD4S-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CFUNC-BDSCO
                    MOVE DCOMWD4S-CTERM
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CTERM
                    MOVE DCOMWD4S-CINDCD-TPO-PSSOA
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO WRK-D4-CINDCD-TPO-PSSOA
                    MOVE LENGTH WRK-AREA-DCOMB0D4
                                       TO DCOMWLOG-E-ANTES-LEN
                    MOVE WRK-AREA-DCOMB0D4
                                       TO DCOMWLOG-E-ANTES-TEXT

                    INITIALIZE WRK-AREA-DCOMB0D4

                    MOVE DCOMWD4E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-NPSSOA-DESC-COML
                    MOVE DCOMWD4E-CCNPJ-CPF
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-NPSSOA-DESC-COML
                    MOVE DCOMWD4E-CFLIAL-CNPJ
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CFLIAL-CNPJ
                    MOVE DCOMWD4E-CCTRL-CNPJ-CPF
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CCTRL-CNPJ-CPF
                    MOVE DCOMWD4E-CDOCTO-ID
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CDOCTO-ID
                    MOVE DCOMWD4E-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-IPSSOA-DESC-COML
                    MOVE DCOMWD4E-CCLUB
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CCLUB
                    MOVE DCOMWD4E-DINCL-REG
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-DINCL-REG
                    MOVE DCOMWD4E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-HULT-ATULZ
                    MOVE DCOMWD4E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CFUNC-BDSCO
                    MOVE DCOMWD4E-CTERM
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CTERM
                    MOVE DCOMWD4E-CINDCD-TPO-PSSOA
                                       OF WRK-AREA-DCOM4D4U
                                       TO WRK-D4-CINDCD-TPO-PSSOA

                    MOVE LENGTH WRK-AREA-DCOMB0D4
                                       TO DCOMWLOG-E-APOS-LEN
                    MOVE WRK-AREA-DCOMB0D4
                                       TO DCOMWLOG-E-APOS-TEXT

               WHEN 05
      **-->         05 - EXCLUIR TESTEMUNHA
                    MOVE 'E'           TO DCOMWLOG-E-CINDCD-MANUT-REG
                    MOVE DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9D
                                       TO HX-NUM-09-CS

                    STRING DCOMWA1I-E-CBCO
                           DCOMWA1I-E-CAG-BCRIA
                           HX-NUM-09-SS
                    DELIMITED BY SIZE  INTO DCOMWLOG-E-CCHAVE-ELMTO-DESC

                    MOVE SPACES        TO WRK-AREA-DCOMB0K9
                    MOVE DCOMWK9E-CBCO OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-CBCO
                    MOVE DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-CAG-BCRIA
                    MOVE DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-NPSSOA-DESC-COML
                    MOVE DCOMWK9E-DULT-UTILZ
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-DULT-UTILZ
                    MOVE DCOMWK9E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-HULT-ATULZ
                    MOVE DCOMWK9E-DINCL-REG
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-DINCL-REG
                    MOVE DCOMWK9E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-CFUNC-BDSCO
                    MOVE DCOMWK9E-CTERM
                                       OF WRK-AREA-DCOM4K9D
                                       TO WRK-K9-CTERM

                    MOVE LENGTH WRK-AREA-DCOMB0K9
                                       TO DCOMWLOG-E-ANTES-LEN
                    MOVE WRK-AREA-DCOMB0K9
                                       TO DCOMWLOG-E-ANTES-TEXT
           END-EVALUATE.

           MOVE 'DCOM3ALL'             TO DCOMWLOG-E-CTRANS-PROG-ATULZ
           MOVE 'O'                    TO DCOMWLOG-E-CINDCD-MODLD-PROCM
           MOVE DCOMWA1I-E-CBCO        TO DCOMWLOG-E-CEMPR-INC
           MOVE DCOMWA1I-CODIGO-DEPENDENCIA
                                       TO DCOMWLOG-E-CDEPDC
           MOVE DCOMWA1I-CAUTEN-SEGRC-N
                                       TO DCOMWLOG-E-CFUNC-BDSCO
           MOVE DCOMWA1I-CTERM         TO DCOMWLOG-E-CTERM

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3LOG)
                COMMAREA (WRK-AREA-DCOM3LOG)
                LENGTH   (LENGTH OF WRK-AREA-DCOM3LOG)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0250'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO  OF WRK-AREA-DCOM3LOG
               WHEN ZEROS
                    CONTINUE

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM3LOG
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM3LOG
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM3LOG              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ALTERAR DADOS DA TESTEMUNHA                                    *
      *----------------------------------------------------------------*
       2400-ALTERAR-TESTEMUNHA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2220-OBTER-DADOS-PESSOA-CPF.

           MOVE '2400-ALTERAR-TESTEMUNHA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-TEM-CADASTRO        EQUAL 'N'
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0260'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0035'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               STRING 'TESTEMUNHA NAO CADASTRADA, CPF: '
                       DCOMWA1I-E-CCPF '-'
                       DCOMWA1I-E-CCTRL-CPF '. '
                      'IMPOSSIVEL SEGUIR COM A ALTERACAO.'
               DELIMITED BY SIZE       INTO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2410-ALTERAR-DADOS-TESTEMUNHA.

           PERFORM 2340-TRATAR-LOG.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> ALTERACAO EFETUADA COM SUCESSO
           MOVE 'DCOM0073'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ALTERAR DADOS DA TESTEMUNHA                                    *
      *----------------------------------------------------------------*
       2410-ALTERAR-DADOS-TESTEMUNHA   SECTION.
      *----------------------------------------------------------------*

           MOVE '2410-ALTERAR-DADOS-TESTEMUNHA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWD4E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4D4U
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4U
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4U.

           MOVE DCOMWD4S-OCORRENCIA    OF WRK-AREA-DCOM4D4S(1)
             TO DCOMWD4E-ENTRADA       OF WRK-AREA-DCOM4D4U

           MOVE DCOMWD4S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO DCOMWD4E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4U.
           MOVE DCOMWA1I-E-IPSSOA-DESC-COML
                                       TO DCOMWD4E-IPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4U.
           MOVE DCOMWA1I-E-CDOCTO-ID   TO DCOMWD4E-CDOCTO-ID
                                       OF WRK-AREA-DCOM4D4U.
           MOVE WRK-TIMESTAMP-ATUAL    TO DCOMWD4E-HULT-ATULZ
                                       OF WRK-AREA-DCOM4D4U.
           MOVE DCOMWA1I-CAUTEN-SEGRC-N
                                       TO DCOMWD4E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4D4U.
           MOVE DCOMWA1I-CTERM         TO DCOMWD4E-CTERM
                                       OF WRK-AREA-DCOM4D4U.
           MOVE 'PARCIAL-01'           TO DCOMWD4E-INSTRUCAO
                                       OF WRK-AREA-DCOM4D4U.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4D4U)
                COMMAREA (WRK-AREA-DCOM4D4U)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4D4U)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                   NOT EQUAL DFHRESP(NORMAL))
               MOVE '0270'               TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4D4U
               WHEN ZEROS
                    CONTINUE

               WHEN 03
                    MOVE 08              TO DCOMW002-COD-RETORNO
                                         OF DFHCOMMAREA
                    MOVE '0280'          TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
                    MOVE 'DCOM0035'      TO DCOMW002-COD-MENSAGEM
                                         OF DFHCOMMAREA
                    MOVE DCOMWD4S-NPSSOA-DESC-COML
                                         OF WRK-AREA-DCOM4D4S(1)
                                         TO HX-NUM-09-CS
                    STRING 'TESTEMUNHA INEXISTENTE OU NAO ENCONTRADA ( '
                            DCOMWA1I-E-CAG-BCRIA '/'
                            HX-NUM-09-SS ' )'
                    DELIMITED BY SIZE  INTO DCOMW002-PARAMETRO
                                         OF DFHCOMMAREA
                    PERFORM 3000-FINALIZAR

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4D4U
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4D4U
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4D4U              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> ALTERACAO EFETUADA COM SUCESSO
           MOVE 'DCOM0073'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * EXCLUIR TESTEMUNHA DA AGENCIA                                  *
      *----------------------------------------------------------------*
       2500-EXCLUIR-TESTEMUNHA         SECTION.
      *----------------------------------------------------------------*

           MOVE '2500-EXCLUIR-TESTEMUNHA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2220-OBTER-DADOS-PESSOA-CPF.

           IF  WRK-TEM-CADASTRO        EQUAL 'N'
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0290'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0035'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               STRING 'TESTEMUNHA NAO CADASTRADA, CPF: '
                       DCOMWA1I-E-CCPF '-'
                       DCOMWA1I-E-CCTRL-CPF '. '
                      'IMPOSSIVEL SEGUIR COM A EXCLUSAO.'
               DELIMITED BY SIZE       INTO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2510-EXCLUIR-TESTEMUNHA-AG.

           PERFORM 2340-TRATAR-LOG.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> EXCLUSAO EFETUADA COM SUCESSO
           MOVE 'DCOM0070'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * EXCLUIR TESTEMUNHA DA AGENCIA                                  *
      *----------------------------------------------------------------*
       2510-EXCLUIR-TESTEMUNHA-AG      SECTION.
      *----------------------------------------------------------------*

           MOVE '2510-EXCLUIR-TESTEMUNHA-AG'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE DCOMWK9E-BLOCO-ENTRADA    OF WRK-AREA-DCOM4K9D
                      DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9D
                      DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9D.

           MOVE DCOMWA1I-E-CBCO        TO DCOMWK9E-CBCO
                                       OF WRK-AREA-DCOM4K9D
           MOVE DCOMWA1I-E-CAG-BCRIA   TO DCOMWK9E-CAG-BCRIA
                                       OF WRK-AREA-DCOM4K9D
           MOVE DCOMWD4S-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4D4S(1)
                                       TO DCOMWK9E-NPSSOA-DESC-COML
                                       OF WRK-AREA-DCOM4K9D
           MOVE DCOMWA1I-CAUTEN-SEGRC-N
                                       TO DCOMWK9E-CFUNC-BDSCO
                                       OF WRK-AREA-DCOM4K9D.
           MOVE DCOMWA1I-CTERM         TO DCOMWK9E-CTERM
                                       OF WRK-AREA-DCOM4K9D.
           MOVE 'TOTAL'                TO DCOMWK9E-INSTRUCAO
                                       OF WRK-AREA-DCOM4K9D

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM4K9D)
                COMMAREA (WRK-AREA-DCOM4K9D)
                LENGTH   (LENGTH OF WRK-AREA-DCOM4K9D)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                   NOT EQUAL DFHRESP(NORMAL))
               MOVE '0300'               TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO OF WRK-AREA-DCOM4K9D
               WHEN ZEROS
                    CONTINUE

               WHEN 03
                    MOVE 08              TO DCOMW002-COD-RETORNO
                                         OF DFHCOMMAREA
                    MOVE '0310'          TO DCOMW002-COD-ERRO
                                         OF DFHCOMMAREA
                    MOVE 'DCOM0035'      TO DCOMW002-COD-MENSAGEM
                                         OF DFHCOMMAREA
                    MOVE DCOMWD4S-NPSSOA-DESC-COML
                                         OF WRK-AREA-DCOM4D4S(1)
                                         TO HX-NUM-09-CS
                    STRING 'TESTEMUNHA INEXISTENTE OU NAO ENCONTRADA ( '
                            DCOMWA1I-E-CAG-BCRIA '/'
                            HX-NUM-09-SS ' )'
                    DELIMITED BY SIZE  INTO DCOMW002-PARAMETRO
                                         OF DFHCOMMAREA
                    PERFORM 3000-FINALIZAR

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM4K9D
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM4K9D
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM4K9D              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> EXCLUSAO EFETUADA COM SUCESSO
           MOVE 'DCOM0070'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE FINALIZACAO DO PROGRAMA                              *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  DCOMW002-COD-MENSAGEM   OF DFHCOMMAREA
                                       EQUAL 'DCOM0035' OR 'DCOM0091'
               MOVE 'S'                TO DCOMW002-MSG-VARIAVEL
                                       OF DFHCOMMAREA
           END-IF.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GERACAO DE LOG DE ERRO CICS.                       *
      *----------------------------------------------------------------*
       9200-TRATAR-ERRO-CICS           SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-CICS              TO TRUE.

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE 16                     TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.

      *--> ERRO CICS
           MOVE 'DCOM0010'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE EIBFN                  TO FRWKGCIC-EIBFN.
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE.
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE.
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP.
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2.
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN.
           MOVE WRK-AREA-ERRO-CICS     TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9999-CHAMAR-API-ERROS.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GERACAO DE LOG DE ERRO DE MODULO.                  *
      *----------------------------------------------------------------*
       9300-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-MODULO            TO TRUE.

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-MODULO             TO FRWKGMOD-NOME-MODULO.
           MOVE DCOMW002-BLOCO-RETORNO OF DFHCOMMAREA
                                       TO FRWKGMOD-BLOCO-RETORNO.
           MOVE WRK-AREA-ERRO-MODULO   TO WRK-BLOCO-INFO-ERRO
                                         (1:FRWKGHEA-TAM-DADOS).

           PERFORM 9999-CHAMAR-API-ERROS.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PROCESSAMENTO DE ERRO LIVRE                                    *
      *----------------------------------------------------------------*
       9500-TRATAR-ERRO-LIVRE          SECTION.
      *----------------------------------------------------------------*

           MOVE '9500-TRATAR-ERRO-LIVRE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           SET ERRO-LIVRE              OF WRK-AREA-ERRO
                                       TO TRUE.

           MOVE DCOMW002-COD-MENSAGEM  OF DFHCOMMAREA
                                       TO FRWKGLIV-COD-MENSAGEM
           MOVE 00001                  TO FRWKGLIV-IDIOMA
           MOVE DCOMWA1I-REGISTRO      TO FRWKGLIV-PARAMETROS.

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-LIVRE    TO WRK-BLOCO-INFO-ERRO
                                       (1:LENGTH OF WRK-AREA-ERRO-LIVRE)

           PERFORM 9999-CHAMAR-API-ERROS.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA GRAVACAO DO LOG DE ERRO DO PROCESSAMENTO.          *
      *----------------------------------------------------------------*
       9999-CHAMAR-API-ERROS           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                PROGRAM (WRK-FRWK1999)
                COMMAREA(WRK-AREA-ERRO)
                LENGTH  (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               CONTINUE
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
