      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3ANI.
       AUTHOR.     FABIO AUGUSTO FINK.
      *================================================================*
      *                        C A P G E M I N I                       *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM3ANI                                      *
      *    PROGRAMADOR.: FABIO AUGUSTO FINK                            *
      *    ANALISTA....: RICARDO JAMMAL                                *
      *    DATA........: 28/10/2024                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: FUNCIONAL P/ GERACAO DO CONTRATO ARQUITETURA. *
      *                 (PARTE DA CONTRATACAO DA OPERACAO).            *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMW002 - BOOK DE ERROS PARA COMUNICACAO ENTRE MODULOS     *
      *    DCOMWAAI - BOOK DE INTERFACE FUNCIONAL DCOM3AAC             *
      *    DCOMWANI - BOOK DE INTERFACE ENTRE COORDENADOR E FUNCIONAL  *
      *                                                                *
      *    BOOKS ESTRUTURAIS:                                          *
      *    DCOMWNSI - BOOK DE INTERFACE ESTRUTURAL DCOM3NSI            *
      *    I#DCOMHX - AREA PARA VARIAVEIS AUXILIARES PARA HEXAVISION   *
      *                                                                *
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKDB COMMAREA FRWK1999 (LOG DE ERROS DB2)               *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    DCOM3AAC - VERIFICAR GRADE HORARIO DO SISTEMA DCOM.         *
      *    DCOM3NSI - OBTER PROXIMO NUMERO SEQUENCIAL PARA O ELEMENTO. *
      *               INFORMADO.                                       *
      *    FRWK1999 - PROCEI2MENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING DCOM3ANI ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-FRWK1999                PIC  X(008)         VALUE
           'FRWK1999'.
       01  WRK-PROGRAMA                PIC  X(008)         VALUE
           'DCOM3ANI'.

       01  WRK-AUXILIARES.
           05 WRK-MODULO               PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA AUXILIAR PARA HEXAVISION ***'.
      *----------------------------------------------------------------*

           COPY 'I#DCOMHX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM3AAC ***'.
      *----------------------------------------------------------------*

       01  WRK-DCOM3AAC                PIC  X(008)         VALUE
           'DCOM3AAC'.

       01  WRK-AREA-DCOM3AAC.
           COPY DCOMW000.
           COPY DCOMWAAI.

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
           '*** AREA DE TRATAMENTO DE ERROS CICS ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.

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
           '*** DCOM3ANI - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           COPY DCOMW002.
           COPY DCOMWANI.

      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
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
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.

           INITIALIZE FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO
                      FRWKGERR-REGISTRO
                      FRWKGHEA-REGISTRO
                      DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                      DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA.

           INITIALIZE DCOMWANI-BLOCO-SAIDA
                         REPLACING ALPHANUMERIC BY SPACES
                                        NUMERIC BY ZEROS.

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           IF  DCOMWANI-E-CPPSTA-PRODT NOT NUMERIC OR
               DCOMWANI-E-CPPSTA-PRODT EQUAL ZEROS
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0010'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CPPSTA-PRODT'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWANI-E-NSMULA-OPER-DESC NOT NUMERIC OR
               DCOMWANI-E-NSMULA-OPER-DESC EQUAL ZEROS
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0015'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-NSMULA-OPER-DESC'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWANI-E-CAG-BCRIA    NOT NUMERIC
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0020'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CAG-BCRIA'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWANI-E-CCTA-BCRIA-CLI  NOT NUMERIC
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0025'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CCTA-BCRIA-CLI'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

4S2511*    IF  DCOMWANI-E-CCNPJ-CPF    NOT NUMERIC
4S2511     IF  DCOMWANI-E-CCNPJ-CPF    EQUAL SPACES OR
4S2511         DCOMWANI-E-CCNPJ-CPF    EQUAL LOW-VALUES
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0030'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CCNPJ-CPF'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

4S2511*    IF  DCOMWANI-E-CFLIAL-CNPJ  NOT NUMERIC
4S2511     IF  DCOMWANI-E-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511         DCOMWANI-E-CFLIAL-CNPJ  EQUAL LOW-VALUES
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0035'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CFLIAL-CNPJ'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  DCOMWANI-E-CCTRL-CNPJ-CPF  NOT NUMERIC
               MOVE 08                 TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0040'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
      *-->     VALOR DO CAMPO <#001,120#> INVALIDO<
               MOVE 'DCOM0091'         TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE 'DCOMWANI-E-CCTRL-CNPJ-CPF'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-VALIDAR-GRADE-HORARIA.

           PERFORM 2200-GERAR-CONTRATO-ARQUIT.

           MOVE ZEROS                  TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA.
           MOVE '0000'                 TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA.
      *--> INCLUSAO EFETUADA COM SUCESSO
           MOVE 'DCOM0045'             TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VERIFICAR SE O SISTEMA DCOM ESTA ABERTO PARA PROCESSAMENTO  *
      *----------------------------------------------------------------*
       2100-VALIDAR-GRADE-HORARIA      SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-VALIDAR-GRADE-HORARIA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE DCOMWAAI-BLOCO-ENTRADA.

           MOVE 'M'                    TO DCOMWAAI-E-TP-PROCESS.
           MOVE 'I'                    TO DCOMWAAI-E-TP-OPCAO.
           MOVE 002                    TO DCOMWAAI-E-TP-FUNCAO.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3AAC)
                COMMAREA (WRK-AREA-DCOM3AAC)
                LENGTH   (LENGTH       OF WRK-AREA-DCOM3AAC)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0225'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           EVALUATE DCOMW000-COD-RETORNO OF WRK-AREA-DCOM3AAC
               WHEN ZEROS
                    IF DCOMWAAI-S-SITUACAO-GRADE
                                       NOT EQUAL 01
                       MOVE 08         TO DCOMW002-COD-RETORNO
                                       OF DFHCOMMAREA
                       MOVE '0230'     TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
                       MOVE 'DCOM0067' TO DCOMW002-COD-MENSAGEM
                                       OF DFHCOMMAREA
                       MOVE 'SISTEMA INDISPONIVEL NO MOMENTO'
                                       TO DCOMW002-PARAMETRO
                                       OF DFHCOMMAREA
                       PERFORM 3000-FINALIZAR
                    END-IF

               WHEN OTHER
                    MOVE DCOMW000-BLOCO-RETORNO OF WRK-AREA-DCOM3AAC
                      TO DCOMW002-BLOCO-RETORNO OF DFHCOMMAREA
                    MOVE 'ERRO AO VERIFICAR GRADE HORARIA DO DCOM'
                      TO DCOMW002-PARAMETRO     OF DFHCOMMAREA
                    MOVE WRK-DCOM3AAC           TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GERAR NUMERO DE CONTRATO DA NOVA ARQUITETURA                *
      *----------------------------------------------------------------*
       2200-GERAR-CONTRATO-ARQUIT      SECTION.
      *----------------------------------------------------------------*

           MOVE '2200-GERAR-CONTRATO-ARQUIT'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE DCOMWNSI-BLOCO-ENTRADA.

           MOVE 35                     TO DCOMWNSI-E-CELMTO-DESC-COML.
           MOVE 1                      TO DCOMWNSI-E-WCHAVE-PRINC-ELMTO.
           MOVE 10                     TO DCOMWNSI-E-QDIG-SEQ-ELMTO.

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM3NSI)
                COMMAREA (WRK-AREA-DCOM3NSI)
                LENGTH   (LENGTH       OF WRK-AREA-DCOM3NSI)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0235'             TO DCOMW002-COD-ERRO
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF.

           EVALUATE DCOMW002-COD-RETORNO   OF WRK-AREA-DCOM3NSI
               WHEN ZEROS
                    CONTINUE

               WHEN OTHER
                    MOVE DCOMW002-BLOCO-RETORNO    OF WRK-AREA-DCOM3NSI
                      TO DCOMW002-BLOCO-RETORNO    OF DFHCOMMAREA
                    MOVE DCOMW002-BLOCO-PARAMETROS OF WRK-AREA-DCOM3NSI
                      TO DCOMW002-BLOCO-PARAMETROS OF DFHCOMMAREA
                    MOVE WRK-DCOM3NSI              TO WRK-MODULO
                    PERFORM 9300-TRATAR-ERRO-MODULO

           END-EVALUATE.

           MOVE 2269651                TO DCOMWANI-S-CPSSOA-JURID-CONTR.
           MOVE 55                     TO DCOMWANI-S-CTPO-CONTR-NEGOC.
           MOVE DCOMWNSI-S-NULT-SEQ-ELMTO TO HX-NRO-18.
           MOVE HX-NRO-10              TO DCOMWANI-S-NSEQ-CONTR-NEGOC.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR O PROCESSAMENTO.                      *
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
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-CHAMAR-API-ERROS           SECTION.
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

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
