      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     DCOM327C.
       AUTHOR.                         PATRICK SCARAMAL.
      *================================================================*
      *    PROGRAMA....: DCOM327C                                      *
      *    PROGRAMADOR.: PATRICK SCARAMAL         - GP 494  - WIPRO    *
      *    ANALISTA....: MARCO AURELIO NOGUEIRA   - GP 439  - DDS      *
      *    DATA........: 20/10/2017                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: IDENTIFICAR FORMULARIO QUE SERA IMPRESSO E    *
      *                  GERAR TOKEN PARA IMPRESSAO IMPD               *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *----------------------------------------------------------------*
      *    -FRWK1999 - ROTINA PADRAO DE GESTAO DE ERROS.               *
      *    -CALE1000 - MODULO DE CALENDARIO                            *
      *    -DICD1002 - OBTER ESTRUTURA DO DICD                         *
      *    -DCOM381A - OBTER DADOS PARA O FORMULARIO DC009             *
      *    -DCOM382A - OBTER DADOS PARA O FORMULARIO DC001             *
      *    -DCOM383A - OBTER DADOS PARA O FORMULARIO DC002             *
      *    -DCOM384A - OBTER DADOS PARA O FORMULARIO DC003             *
      *    -DCOM385A - OBTER DADOS PARA O FORMULARIO DC004             *
      *    -DCOM386A - OBTER DADOS PARA O FORMULARIO DC005             *
      *    -DCOM387A - OBTER DADOS PARA O FORMULARIO DC007             *
      *    -DCOM388A - OBTER DADOS PARA O FORMULARIO DC006             *
      *    -DCOM389A - OBTER DADOS PARA O FORMULARIO DC008             *
      *----------------------------------------------------------------*
      *    COPYBOOKS:                                                  *
      *    -I#FRWKGE - AREA DE COMUNICACAO FRWK1999.                   *
      *    -I#FRWKMD - AREA PARA TRATAMENTO DE ERROS MODULOS.          *
      *    -I#FRWKCI - AREA PARA TRATAMENTO DE ERROS CICS.             *
      *    -I#IMPDB1 COMMAREA ENTRE PROGRAMA/FRAMEWORK                 *
      *    -FRWKWAAC COMMAREA PARA OBTER DADOS DA SESSAO               *
      *    -DCOMW000 - AREA DE RETORNO DE ERROS DO MODULO FUNCIONAL.   *
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-VARIAVEIS-AUXILIARES.
           05  WRK-PROGRAMA           PIC  X(008)      VALUE 'DCOM327C'.
           05  WRK-CALE1000           PIC  X(008)      VALUE 'CALE1000'.
           05  WRK-DICD1002           PIC  X(008)      VALUE 'DICD1002'.
           05  WRK-DCOM381A           PIC  X(008)      VALUE 'DCOM381A'.
           05  WRK-DCOM382A           PIC  X(008)      VALUE 'DCOM382A'.
           05  WRK-DCOM383A           PIC  X(008)      VALUE 'DCOM383A'.
           05  WRK-DCOM384A           PIC  X(008)      VALUE 'DCOM384A'.
           05  WRK-DCOM385A           PIC  X(008)      VALUE 'DCOM385A'.
           05  WRK-DCOM386A           PIC  X(008)      VALUE 'DCOM386A'.
           05  WRK-DCOM387A           PIC  X(008)      VALUE 'DCOM387A'.
           05  WRK-DCOM388A           PIC  X(008)      VALUE 'DCOM388A'.
           05  WRK-DCOM389A           PIC  X(008)      VALUE 'DCOM389A'.
           05  WRK-FRWK1999           PIC  X(008)      VALUE 'FRWK1999'.

       01  IND-1                       PIC 9(003) COMP-3   VALUE ZEROS.

       01  WRK-DATA-ATU               PIC  X(010)      VALUE SPACES.

       01  WRK-DATA-PTO               PIC  X(010)      VALUE SPACES.

       01  WRK-FORMULARIO             PIC  X(008)      VALUE SPACES.

       01  WRK-DOCTO                  PIC  X(008)      VALUE SPACES.

       01  WRK-9-15-V99               PIC  9(15)V99       VALUE ZEROS.
       01  WRK-9-17 REDEFINES WRK-9-15-V99
                                      PIC  9(17).

       01  WRK-9-03-V99               PIC  9(03)V99       VALUE ZEROS.
       01  WRK-9-05 REDEFINES WRK-9-03-V99
                                      PIC  9(05).

       01  WRK-9-07-V9-6              PIC  9(07)V9(6)     VALUE ZEROS.
       01  WRK-9-13 REDEFINES WRK-9-07-V9-6
                                      PIC  9(13).

       01  WRK-9-13-V99               PIC  9(13)V99       VALUE ZEROS.
       01  WRK-9-15 REDEFINES WRK-9-13-V99
                                      PIC  9(15).

       01  WRK-9-8-V9-7               PIC  9(08)V9(7)     VALUE ZEROS.
       01  WRK-9-15-2 REDEFINES WRK-9-8-V9-7
                                      PIC  9(15).

       01  WRK-9-9-V9-6               PIC  9(09)V9(6)     VALUE ZEROS.
       01  WRK-9-15-4 REDEFINES WRK-9-9-V9-6
                                      PIC  9(15).

       01  WRK-9-15-V99-2             PIC  9(15)V9(2)     VALUE ZEROS.
       01  FILLER REDEFINES WRK-9-15-V99-2.
           05 FILLER                  PIC  X(02).
           05 WRK-9-15-3              PIC  9(15).

       01  WRK-X-19-CPF                PIC  X(19)         VALUE SPACES.
       01  FILLER REDEFINES WRK-X-19-CPF.
4S2511*      05 WRK-9-9-CPF             PIC  9(09).
4S2511       05 WRK-9-9-CPF             PIC  X(09).
           05 WRK-X-HIF               PIC  X(01).
           05 WRK-9-2-CPF             PIC  9(02).
           05 FILLER                  PIC  X(07).

       01  WRK-X-19-CNPJ               PIC  X(19)         VALUE SPACES.
       01  FILLER REDEFINES WRK-X-19-CNPJ.
4S2511*      05 WRK-9-9-CNPJ            PIC  9(09).
4S2511       05 WRK-9-9-CNPJ            PIC  X(09).
           05 WRK-X-BARRA             PIC  X(01).
4S2511*      05 WRK-9-4-CNPJ            PIC  9(04).
4S2511       05 WRK-9-4-CNPJ            PIC  X(04).
           05 WRK-X-HIF2              PIC  X(01).
           05 WRK-9-2-CNPJ            PIC  9(02).
           05 FILLER                  PIC  X(02).

       01  WRK-X-19-GENER              PIC  X(15)         VALUE SPACES.
       01  FILLER REDEFINES WRK-X-19-GENER.
4S2511*      05 WRK-9-9-GENER           PIC  9(09).
4S2511       05 WRK-9-9-GENER           PIC  X(09).
4S2511*      05 WRK-9-4-GENER           PIC  9(04).
4S2511       05 WRK-9-4-GENER           PIC  X(04).
           05 WRK-9-2-GENER           PIC  9(02).

       01  WRK-OPERACAO-AUX           PIC  9(13)          VALUE ZEROS.
       01  FILLER REDEFINES WRK-OPERACAO-AUX.
           05 WRK-ANO-OPER            PIC  9(04).
           05 WRK-NUM-CONTR           PIC  9(09).
      *
      *----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO.
               10  WRK-CHAR-INFO-ERRO  PIC  X(001)
                             OCCURS 526   TIMES.
      *
      *----------------------------------------------------------------*
      *    'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-CICS.
           COPY 'I#FRWKCI'.
      *
      *----------------------------------------------------------------*
      *    'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-MOD.
           COPY 'I#FRWKMD'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE SESSAO'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-SESSAO.
           COPY FRWKWAAC.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO DO PROGRAMA DICD1002'.
      *----------------------------------------------------------------*
       01  WRK-AREA-DICD1002.
           COPY 'I#DICD52'.
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO CALE1000'.
      *----------------------------------------------------------------*
      *
           COPY CALEWAAA.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE CHAVE DE IDENTIFICACAO DO USUARIO'.
      *----------------------------------------------------------------*
       01  WRK-DCOMW90A.
           COPY DCOMW90A.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM381A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM381A.
           COPY DCOMW000.
           COPY DCOMW81I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM382A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM382A.
           COPY DCOMW000.
           COPY DCOMW82I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM383A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM383A.
           COPY DCOMW000.
           COPY DCOMW83I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM384A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM384A.
           COPY DCOMW000.
           COPY DCOMW84I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM385A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM385A.
           COPY DCOMW000.
           COPY DCOMW85I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM386A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM386A.
           COPY DCOMW000.
           COPY DCOMW86I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM387A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM387A.
           COPY DCOMW000.
           COPY DCOMW87I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM388A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM388A.
           COPY DCOMW000.
           COPY DCOMW88I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO DCOM389A'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-DCOM389A.
           COPY DCOMW000.
           COPY DCOMW89I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 4840-363E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW81A.
           COPY DCOMW81A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 1255-6E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW82A.
           COPY DCOMW82A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 1105-3E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW83A.
           COPY DCOMW83A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 1025-1E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW84A.
           COPY DCOMW84A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 4840-439E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW85A.
           COPY DCOMW85A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 4840-437E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW86A.
           COPY DCOMW86A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 1704-3E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW87A.
           COPY DCOMW87A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 1256-4E'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW88A.
           COPY DCOMW88A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(070)         VALUE
           'AREA PARA GERACAO DA IMAGEM DA IMPRESSAO - 4840-569'.
      *----------------------------------------------------------------*
      *
       01  WRK-DCOMW89A.
           COPY DCOMW89A.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ENVIO AO IMPD'.
      *----------------------------------------------------------------*
       01  WRK-AREA-IMPD1201.
           COPY 'I#IMPDB1'.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY DCOMW000.
           COPY DCOMW27I.
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 9000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE        FRWKGHEA-REGISTRO
                             FRWKGCIC-REGISTRO
                             FRWKGMOD-REGISTRO
                             FRWKGERR-REGISTRO
                             IMPDGLB1-REGISTRO
                             DCOMW27I-BLOCO-SAIDA.
      *
           PERFORM 1050-CONSISTIR-ENTRADA
      *
           PERFORM 1200-BUSCAR-DATA-ATUAL.
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1050-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           IF  DCOMW27I-E-COD-DOCTO    EQUAL LOW-VALUES OR
               DCOMW27I-E-COD-DOCTO    EQUAL SPACES
               MOVE 08                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0010'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0028'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9000-FINALIZAR
           END-IF.

           IF  DCOMW27I-E-NUM-OPER     NOT NUMERIC  OR
               DCOMW27I-E-NUM-OPER     EQUAL ZEROS
               MOVE 08                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0020'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0004'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1200-BUSCAR-DATA-ATUAL          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '1200-BUSCAR-DATA-ATUAL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE CALEWAAA-BLOCO-RETORNO
                      CALEWAAA-SAIDA
      *
           EXEC CICS LINK
                PROGRAM  (WRK-CALE1000)
                COMMAREA (CALEWAAA)
                LENGTH   (LENGTH OF CALEWAAA)
                NOHANDLE
           END-EXEC
      *
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0030'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF CALEWAAA-COD-RET         NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0040'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-CALE1000        TO FRWKGMOD-NOME-MODULO
              MOVE CALEWAAA-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF

           MOVE CALEWAAA-DT-GREGO-P-DDMMAAAA
                                       TO WRK-DATA-ATU.
      *
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EVALUATE DCOMW27I-E-COD-DOCTO
              WHEN '4840-363E'
                 PERFORM 2100-BUSCAR-DADOS-FORM-W81A
              WHEN '1255-6E'
                 PERFORM 2200-BUSCAR-DADOS-FORM-W82A
              WHEN '1255-6'
                 MOVE '1255-6E'  TO DCOMW27I-E-COD-DOCTO
                 PERFORM 2200-BUSCAR-DADOS-FORM-W82A
              WHEN '1105-3E'
                 PERFORM 2300-BUSCAR-DADOS-FORM-W83A
              WHEN '1105-3'
                 MOVE '1105-3E'  TO DCOMW27I-E-COD-DOCTO
                 PERFORM 2300-BUSCAR-DADOS-FORM-W83A
              WHEN '1025-1E'
                 PERFORM 2400-BUSCAR-DADOS-FORM-W84A
              WHEN '4840-439E'
                 PERFORM 2500-BUSCAR-DADOS-FORM-W85A
              WHEN '4840-437E'
                 PERFORM 2600-BUSCAR-DADOS-FORM-W86A
              WHEN '1704-3E'
                 PERFORM 2700-BUSCAR-DADOS-FORM-W87A
              WHEN '1256-4E'
                 PERFORM 2800-BUSCAR-DADOS-FORM-W88A
              WHEN '1256-4'
                 MOVE '1256-4E'  TO DCOMW27I-E-COD-DOCTO
                 PERFORM 2800-BUSCAR-DADOS-FORM-W88A
              WHEN '4840-569E'
                 PERFORM 2900-BUSCAR-DADOS-FORM-W89A
              WHEN OTHER
                 MOVE 08          TO DCOMW000-COD-RETORNO OF DFHCOMMAREA
                 MOVE '0050'         TO DCOMW000-COD-ERRO OF DFHCOMMAREA
                 MOVE 'DCOM0015' TO DCOMW000-COD-MENSAGEM OF DFHCOMMAREA
                 PERFORM 9000-FINALIZAR
           END-EVALUATE

           PERFORM 6000-OBTER-ESTRUTURA.

           PERFORM 3000-PREENCHER-IMPD

           MOVE WRK-DCOMW90A           TO DCOMW27I-S-CHAVE
      *
           MOVE IMPDGLB1-REGISTRO      TO DCOMW27I-S-IMPD
      *
           MOVE 00                     TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
           MOVE '0000'                 TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
           MOVE 'DCOM0002'             TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2100-BUSCAR-DADOS-FORM-W81A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ACESSAR-DCOM381A

           PERFORM 2120-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM381A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2110-ACESSAR-DCOM381A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW81I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW81I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM381A)
                COMMAREA (WRK-AREA-DCOM381A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM381A)
                NOHANDLE
           END-EXEC
      *
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0060'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM381A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0070'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM381A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM381A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW81A'                 TO WRK-FORMULARIO
           MOVE DCOMW81I-S-CAG-BCRIA       TO DCOMW81A-CAG-BCRIA
           MOVE DCOMW81I-S-DIGITO-AGENCIA  TO DCOMW81A-DIGITO-AGENCIA
           MOVE DCOMW81I-S-CCTA-BCRIA-CLI  TO DCOMW81A-CCTA-BCRIA-CLI
           MOVE DCOMW81I-S-DIGITO-CONTA    TO DCOMW81A-DIGITO-CONTA
           MOVE DCOMW81I-S-CCNPJ-CPF       TO DCOMW81A-CCNPJ-CPF
           MOVE DCOMW81I-S-CFLIAL-CNPJ     TO DCOMW81A-CFLIAL-CNPJ
           MOVE DCOMW81I-S-CCTRL-CNPJ-CPF  TO DCOMW81A-CCTRL-CNPJ-CPF
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW81A-OPERACAO
           MOVE DCOMW81I-S-DINIC-OPER-DESC TO DCOMW81A-DINIC-OPER-DESC
           MOVE DCOMW81I-S-VOPER-DESC-COML TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VOPER-DESC-COML
           MOVE DCOMW81I-S-IPSSOA-DESC-COML TO DCOMW81A-IPSSOA-DESC-COML
           MOVE DCOMW81I-S-DATA-PRIM-VCTO  TO DCOMW81A-DATA-PRIM-VCTO
           MOVE DCOMW81I-S-DVCTO-FNAL-OPER TO DCOMW81A-DVCTO-FNAL-OPER
           MOVE DCOMW81I-S-PTX-JURO-MES    TO WRK-9-07-V9-6
           MOVE WRK-9-13                   TO DCOMW81A-PTX-JURO-MES
           MOVE DCOMW81I-S-PTX-JURO-ANO    TO WRK-9-07-V9-6
           MOVE WRK-9-13                   TO DCOMW81A-PTX-JURO-ANO
           MOVE DCOMW81I-S-QTITLO-CHEQ-PCELA
                                           TO DCOMW81A-QTITLO-CHEQ-PCELA

           MOVE DCOMW81I-S-VTRIBUTOS       TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VTRIBUTOS
           MOVE DCOMW81I-S-PTRIBUTOS       TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PTRIBUTOS

           MOVE DCOMW81I-S-VSEGUROS        TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VSEGUROS
           MOVE DCOMW81I-S-PSEGUROS        TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PSEGUROS

           MOVE DCOMW81I-S-VTARIFA-CET     TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VTARIFA-CET
           MOVE DCOMW81I-S-PTARIFA-CET     TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PTARIFA-CET

           MOVE DCOMW81I-S-VPGTOSERV       TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VPGTOSERV

           MOVE DCOMW81I-S-PPGTOSERV       TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PPGTOSERV

           MOVE DCOMW81I-S-VREGISTROS      TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VREGISTROS

           MOVE DCOMW81I-S-PREGISTROS      TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PREGISTROS

           MOVE DCOMW81I-S-VCET-TOTAL      TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VCET-TOTAL

           MOVE DCOMW81I-S-PCET-TOTAL      TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-PCET-TOTAL

           MOVE DCOMW81I-S-VCET-MES        TO WRK-9-8-V9-7
           MOVE WRK-9-15-2                 TO DCOMW81A-VCET-MES

           MOVE DCOMW81I-S-VCET-ANO        TO WRK-9-8-V9-7
           MOVE WRK-9-15-2                 TO DCOMW81A-VCET-ANO

           MOVE DCOMW81I-S-VTOT-LIQ        TO WRK-9-13-V99
           MOVE WRK-9-15                   TO DCOMW81A-VTOT-LIQ

           MOVE DCOMW81I-S-VTOT-LIB        TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW81A-VTOT-LIB

           MOVE DCOMW81I-S-TOT-LIB         TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW81A-TOT-LIB
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW81A-CONTR-COD-BARR
           MOVE  7                         TO DCOMW81A-NUM-COD-BARR
           MOVE CALEWAAA-DT-MMAA-MMAAAA    TO DCOMW81A-MM-AAAA-BARR
           MOVE WRK-DCOMW81A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                 OF WRK-DCOMW81A
                                           TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC009'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-BUSCAR-DADOS-FORM-W82A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ACESSAR-DCOM382A

           PERFORM 2220-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-ACESSAR-DCOM382A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2210-ACESSAR-DCOM382A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW82I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW82I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM382A)
                COMMAREA (WRK-AREA-DCOM382A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM382A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0080'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM382A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0090'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM382A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM382A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW82A'                 TO WRK-FORMULARIO
           MOVE DCOMW82I-S-CAG-BCRIA       TO DCOMW82A-CAG-BCRIA
           MOVE DCOMW82I-S-DIGITO-AGENCIA  TO DCOMW82A-DIGITO-AGEN
           MOVE DCOMW82I-S-CCTA-BCRIA-CLI  TO DCOMW82A-CCTA-BCRIA
           MOVE DCOMW82I-S-DIGITO-CONTA    TO DCOMW82A-DIGITO-CONTA
           MOVE DCOMW82I-S-CCNPJ-CPF       TO DCOMW82A-CCNPJ-CPF
           MOVE DCOMW82I-S-CFLIAL-CNPJ     TO DCOMW82A-CFLIAL-CNPJ
           MOVE DCOMW82I-S-CCTRL-CNPJ-CPF  TO DCOMW82A-CCTRL-CPF
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW82A-OPERACAO
           MOVE DCOMW82I-S-DINIC-OPER-DESC TO DCOMW82A-DINIC-OPER
           MOVE DCOMW82I-S-VOPER-DESC-COML TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VOPER-DESC
           MOVE DCOMW82I-S-IPSSOA-DESC-COML TO DCOMW82A-IPSSOA-DESC
           MOVE DCOMW82I-S-DATA-PRIM-VCTO  TO DCOMW82A-DPRIM-VCTO
           MOVE DCOMW82I-S-DVCTO-FNAL-OPER TO DCOMW82A-DVCTO-FNAL
           MOVE DCOMW82I-S-PTX-JURO-MES    TO WRK-9-07-V9-6
           MOVE WRK-9-13                   TO DCOMW82A-PTX-JURO-MES
           MOVE DCOMW82I-S-PTX-JURO-ANO    TO WRK-9-07-V9-6
           MOVE WRK-9-13                   TO DCOMW82A-PTX-JURO-ANO
           MOVE DCOMW82I-S-QTITLO-CHEQ-PCELA
                                           TO DCOMW82A-QTITLO-CHEQ
           MOVE DCOMW82I-S-CCART-LIM       TO DCOMW82A-CCART-LIM
           MOVE DCOMW82I-S-VTRIBUTOS       TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VTRIBUTOS
           MOVE DCOMW82I-S-PTRIBUTOS       TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-TRIBUTOS
           MOVE DCOMW82I-S-VSEGUROS        TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VSEGUROS

           MOVE DCOMW82I-S-VIOF-OPER       TO WRK-9-15-V99-2
           MOVE WRK-9-15-3                 TO DCOMW82A-VIOF-OPER

           MOVE DCOMW82I-S-VTAC-OPER       TO WRK-9-15-V99-2
           MOVE WRK-9-15-3                 TO DCOMW82A-VTAC-OPER

           MOVE DCOMW82I-S-PSEGUROS        TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-PSEGUROS

           MOVE DCOMW82I-S-VTARIFA-CET     TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VTARIFA-CET

           MOVE DCOMW82I-S-PTARIFA-CET     TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-PTARIFA-CET

           MOVE DCOMW82I-S-VPGTOSERV       TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VPGTOSERV

           MOVE DCOMW82I-S-PPGTOSERV       TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-PPGTOSERV

           MOVE DCOMW82I-S-VREGISTROS      TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VREGISTROS

           MOVE DCOMW82I-S-PREGISTROS      TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-PREGISTROS

           MOVE DCOMW82I-S-VCET-TOTAL      TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VCET-TOTAL

           MOVE DCOMW82I-S-PCET-TOTAL      TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-PCET-TOTAL

           MOVE DCOMW82I-S-VCET-MES        TO WRK-9-8-V9-7
           MOVE WRK-9-15-2                 TO DCOMW82A-VCET-MES

           MOVE DCOMW82I-S-VCET-ANO        TO WRK-9-8-V9-7
           MOVE WRK-9-15-2                 TO DCOMW82A-VCET-ANO

           MOVE DCOMW82I-S-VTOT-LIQ        TO WRK-9-13-V99
           MOVE WRK-9-15                   TO DCOMW82A-VTOT-LIB

           MOVE DCOMW82I-S-VTOT-LIB        TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW82A-VTOT-LIB

           MOVE DCOMW82I-S-TOT-LIB         TO WRK-9-03-V99
           MOVE WRK-9-05                   TO DCOMW82A-TOT-LIB
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW82A-CONTR-NUM-FIXO
                                          OF WRK-DCOMW82A
           MOVE  7                         TO DCOMW82A-CONTRCOD-BARR
                                          OF WRK-DCOMW82A
           MOVE CALEWAAA-DT-MMAA-MMAAAA    TO DCOMW82A-VERSAO-MMAAAA
                                          OF WRK-DCOMW82A

           MOVE DCOMW27I-E-TIPO-VIA        TO DCOMW82A-TIPO-VIA
           MOVE DCOMW82I-S-NOCOR           TO DCOMW82A-NOCOR

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW82I-S-NOCOR OR IND-1 GREATER 04
              MOVE DCOMW82I-S-NOME-AVAL(IND-1)   TO
                                         DCOMW82A-NOME-AVAL(IND-1)
              MOVE DCOMW82I-S-CNPJ-AVAL(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW82I-S-FILIAL-AVAL(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW82I-S-CTRL-AVAL(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER      TO  DCOMW82A-CPFPJ-AVAL(IND-1)
      *
              MOVE DCOMW82I-S-NOME-CONJUGE(IND-1)   TO
                                         DCOMW82A-IPSSOA-CONJG(IND-1)
              MOVE DCOMW82I-S-CNPJ-CNJG(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW82I-S-FILIAL-CNJG(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW82I-S-CTRL-CNJG(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER      TO DCOMW82A-CPFPJ-CONJG(IND-1)
      *
              MOVE DCOMW82I-S-NOME-TEST(IND-1)   TO
                                         DCOMW82A-NOME-TEST(IND-1)
              MOVE DCOMW82I-S-CNPJ-TEST(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW82I-S-FILIAL-TEST(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW82I-S-CTRL-TEST(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER    TO  DCOMW82A-CPFPF-TEST(IND-1)
           END-PERFORM.
      *
           MOVE WRK-DCOMW82A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                     OF WRK-DCOMW82A
                                           TO IMPDGLB1-CONTEUDO-TAM.

           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC001'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.
      *
      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-BUSCAR-DADOS-FORM-W83A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ACESSAR-DCOM383A

           PERFORM 2320-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-ACESSAR-DCOM383A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2310-ACESSAR-DCOM383A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW83I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW83I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM383A)
                COMMAREA (WRK-AREA-DCOM383A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM383A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0100'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM383A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0110'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM383A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM383A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.


      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW83A'                 TO WRK-FORMULARIO
           MOVE  DCOMW83I-S-CAG-BCRIA        TO DCOMW83A-CAG-BCRIA
           MOVE  DCOMW83I-S-DIGITO-AGENCIA   TO DCOMW83A-DIGITO-AGEN
           MOVE  DCOMW83I-S-CCTA-BCRIA-CLI   TO DCOMW83A-CCTA-BCRIA
           MOVE  DCOMW83I-S-DIGITO-CONTA     TO DCOMW83A-DIGITO-CONTA
           MOVE  DCOMW83I-S-CCNPJ-CPF        TO DCOMW83A-CCNPJ-CPF
           MOVE  DCOMW83I-S-CFLIAL-CNPJ      TO DCOMW83A-CFLIAL-CNPJ
           MOVE  DCOMW83I-S-CCTRL-CNPJ-CPF   TO DCOMW83A-CCTRL-CNPJ-CPF
           MOVE  DCOMW27I-E-NUM-OPER         TO DCOMW83A-OPERACAO
           MOVE  DCOMW27I-E-NUM-OPER         TO DCOMW83A-CONTR-COD-BARR
           MOVE  DCOMW83I-S-DINIC-OPER-DESC  TO DCOMW83A-DINIC-OPER
           MOVE  DCOMW83I-S-VOPER-DESC-COML  TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VOPER-DESC
           MOVE  DCOMW83I-S-NOME-AGEN        TO DCOMW83A-NOME-AGEN
           MOVE  DCOMW83I-S-IPSSOA-DESC-COML TO DCOMW83A-IPSSOA-DESC
           MOVE  DCOMW83I-S-VLIQ-OPER        TO WRK-9-13-V99
           MOVE  WRK-9-15                    TO DCOMW83A-VLIQ-OPER
           MOVE  DCOMW83I-S-PTX-JURO-MES     TO WRK-9-07-V9-6
           MOVE  WRK-9-13                    TO DCOMW83A-PTX-JURO-MES
           MOVE  DCOMW83I-S-PTX-JURO-ANO     TO WRK-9-07-V9-6
           MOVE  WRK-9-13                    TO DCOMW83A-PTX-JURO-ANO
           MOVE  DCOMW83I-S-CCART-LIM        TO DCOMW83A-CCART-LIM
           MOVE  DCOMW83I-S-DATA-PRIM-VCTO   TO DCOMW83A-DPRIM-VCTO
           MOVE  DCOMW83I-S-DVCTO-FNAL-OPER  TO DCOMW83A-DVCTO-FNAL
           MOVE  DCOMW83I-S-VNOTA-PROMIS     TO WRK-9-13-V99
           MOVE  WRK-9-15                    TO DCOMW83A-VNOTA-PROMIS
           MOVE  DCOMW83I-S-VIOF-OPER        TO WRK-9-13-V99
           MOVE  WRK-9-15                    TO DCOMW83A-VIOF-OPER
           MOVE  DCOMW83I-S-VTAC-OPER        TO WRK-9-13-V99
           MOVE  WRK-9-15                    TO DCOMW83A-VTAC-OPER
           MOVE  DCOMW83I-S-VTARIF-REG       TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VTARIF-REG
           MOVE  DCOMW83I-S-VTRIBUTOS        TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VTRIBUTOS
           MOVE  DCOMW83I-S-PTRIBUTOS        TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-TRIBUTOS
           MOVE  DCOMW83I-S-VSEGUROS         TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VSEGUROS
           MOVE  DCOMW83I-S-PSEGUROS         TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-PSEGUROS
           MOVE  DCOMW83I-S-VTARIFA-CET      TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VTARIFA-CET
           MOVE  DCOMW83I-S-PTARIFA-CET      TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-PTARIFA-CET
           MOVE  DCOMW83I-S-VPGTOSERV        TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VPGTOSERV
           MOVE  DCOMW83I-S-PPGTOSERV        TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-PPGTOSERV
           MOVE  DCOMW83I-S-VREGISTROS       TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VREGISTROS
           MOVE  DCOMW83I-S-PREGISTROS       TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-PREGISTROS
           MOVE  DCOMW83I-S-VCET-TOTAL       TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VCET-TOTAL
           MOVE  DCOMW83I-S-PCET-TOTAL       TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-PCET-TOTAL
           MOVE  DCOMW83I-S-VCET-MES         TO WRK-9-8-V9-7
           MOVE  WRK-9-15-2                  TO DCOMW83A-VCET-MES
           MOVE  DCOMW83I-S-VCET-ANO         TO WRK-9-8-V9-7
           MOVE  WRK-9-15-2                  TO DCOMW83A-VCET-ANO
           MOVE  DCOMW83I-S-VTOT-LIB         TO WRK-9-15-V99
           MOVE  WRK-9-17                    TO DCOMW83A-VTOT-LIB
           MOVE  DCOMW83I-S-TOT-LIB          TO WRK-9-03-V99
           MOVE  WRK-9-05                    TO DCOMW83A-TOT-LIB
           MOVE  DCOMW83I-S-NOCOR            TO DCOMW83A-NOCOR
           MOVE  7                           TO DCOMW83A-NUM-COD-BARR
           MOVE CALEWAAA-DT-MMAA-MMAAAA      TO DCOMW83A-MM-AAAA-BARR
           MOVE DCOMW27I-E-TIPO-VIA        TO DCOMW83A-TIPO-VIA

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW83I-S-NOCOR OR IND-1 GREATER 04
              MOVE DCOMW83I-S-NOME-AVALISTA(IND-1)   TO
                                         DCOMW83A-NOME-AVAL(IND-1)
              MOVE DCOMW83I-S-CNPJ-AVALISTA(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW83I-S-FILIAL-AVALISTA(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW83I-S-CTRL-AVALISTA(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER      TO  DCOMW83A-CPFPJ-AVAL(IND-1)
      *
              MOVE DCOMW83I-S-NOME-CONJUGE(IND-1)   TO
                                         DCOMW83A-IPSSOA-CONJG(IND-1)
              MOVE DCOMW83I-S-CNPJ-CNJG(IND-1)      TO WRK-9-9-GENER
              MOVE DCOMW83I-S-FILIAL-CNJG(IND-1)    TO WRK-9-4-GENER
              MOVE DCOMW83I-S-FILIAL-CNJG(IND-1)    TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER     TO  DCOMW83A-CPFPJ-CONJG(IND-1)
      *
              MOVE DCOMW83I-S-NOME-TESTEMUNHA(IND-1)   TO
                                         DCOMW83A-NOME-TEST(IND-1)
              MOVE DCOMW83I-S-CNPJ-TESTEMUNHA(IND-1)TO WRK-9-9-GENER
              MOVE DCOMW83I-S-FILIAL-TESTEMUNHA(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW83I-S-CTRL-TESTEMUNHA(IND-1)TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER     TO DCOMW83A-CPFPJ-TEST(IND-1)
      *
           END-PERFORM.

           MOVE WRK-DCOMW83A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH OF WRK-DCOMW83A     TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC002'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.
      *
      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-BUSCAR-DADOS-FORM-W84A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ACESSAR-DCOM384A

           PERFORM 2420-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-ACESSAR-DCOM384A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2410-ACESSAR-DCOM384A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW84I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW84I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM384A)
                COMMAREA (WRK-AREA-DCOM384A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM384A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0120'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM384A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0130'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM384A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM384A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.


      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW84A'                 TO WRK-FORMULARIO.

           MOVE  DCOMW84I-S-CAG-BCRIA           TO DCOMW84A-CAG-BCRIA
           MOVE  DCOMW84I-S-DIGITO-AGENCIA      TO DCOMW84A-DIGITO-AGEN
           MOVE  DCOMW84I-S-CCTA-BCRIA-CLI      TO DCOMW84A-CCTA-BCRIA
           MOVE  DCOMW84I-S-DIGITO-CONTA        TO DCOMW84A-DIGITO-CONTA
           MOVE  DCOMW84I-S-CCNPJ-CPF           TO DCOMW84A-CCNPJ-CPF
           MOVE  DCOMW84I-S-CFLIAL-CNPJ         TO DCOMW84A-CFLIAL-CNPJ
           MOVE  DCOMW84I-S-CCTRL-CNPJ-CPF    TO DCOMW84A-CCTRL-CNPJ-CPF
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW84A-OPERACAO
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW84A-CONTR-COD-BARR
           MOVE  DCOMW84I-S-DINIC-OPER-DESC     TO DCOMW84A-DINIC-OPER
           MOVE  DCOMW84I-S-VOPER-DESC-COML     TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VOPER-DESC
           MOVE  DCOMW84I-S-IPSSOA-DESC-COML    TO DCOMW84A-IPSSOA-DESC
           MOVE  DCOMW84I-S-DATA-PRIM-VCTO      TO DCOMW84A-DPRIM-VCTO
           MOVE  DCOMW84I-S-DVCTO-FNAL-OPER     TO DCOMW84A-DVCTO-FNAL
           MOVE  DCOMW84I-S-PTX-JURO-MES        TO WRK-9-07-V9-6
           MOVE  WRK-9-13                       TO DCOMW84A-PTX-JURO-MES
           MOVE  DCOMW84I-S-PTX-JURO-ANO        TO WRK-9-07-V9-6
           MOVE  WRK-9-13                       TO DCOMW84A-PTX-JURO-ANO
           MOVE  DCOMW84I-S-VIOF-OPER         TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW84A-VIOF-OPER-DESC
           MOVE  DCOMW84I-S-VTAC-OPER         TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW84A-VTAC-OPER-DESC
           MOVE  DCOMW84I-S-QTITLO-CHEQ       TO DCOMW84A-QTITLO-CHEQ
           MOVE  DCOMW84I-S-VAL-OPER-EXT      TO DCOMW84A-VAL-OPER-EXT
           MOVE  DCOMW84I-S-VTRIBUTOS           TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VTRIBUTOS
           MOVE  DCOMW84I-S-PTRIBUTOS           TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-TRIBUTOS
           MOVE  DCOMW84I-S-VSEGUROS            TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VSEGUROS
           MOVE  DCOMW84I-S-PSEGUROS            TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-PSEGUROS
           MOVE  DCOMW84I-S-VTARIFA-CET         TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VTARIFA-CET
           MOVE  DCOMW84I-S-PTARIFA-CET         TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-PTARIFA-CET
           MOVE  DCOMW84I-S-VPGTOSERV           TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VPGTOSERV
           MOVE  DCOMW84I-S-PPGTOSERV           TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-PPGTOSERV
           MOVE  DCOMW84I-S-VREGISTROS          TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VREGISTROS
           MOVE  DCOMW84I-S-PREGISTROS          TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-PREGISTROS
           MOVE  DCOMW84I-S-VCET-TOTAL          TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VCET-TOTAL
           MOVE  DCOMW84I-S-PCET-TOTAL          TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-PCET-TOTAL
           MOVE  DCOMW84I-S-VCET-MES            TO WRK-9-8-V9-7
           MOVE  WRK-9-15-2                     TO DCOMW84A-VCET-MES
           MOVE  DCOMW84I-S-VCET-ANO            TO WRK-9-8-V9-7
           MOVE  WRK-9-15-2                     TO DCOMW84A-VCET-ANO
           MOVE  DCOMW84I-S-VTOT-LIB            TO WRK-9-15-V99
           MOVE  WRK-9-17                       TO DCOMW84A-VTOT-LIB
           MOVE  DCOMW84I-S-TOT-LIB             TO WRK-9-03-V99
           MOVE  WRK-9-05                       TO DCOMW84A-TOT-LIB
           MOVE  DCOMW84I-S-NOCOR               TO DCOMW84A-NOCOR
           MOVE  7                           TO DCOMW84A-NUM-COD-BARR
           MOVE CALEWAAA-DT-MMAA-MMAAAA      TO DCOMW84A-MM-AAAA-BARR
           MOVE DCOMW27I-E-TIPO-VIA        TO DCOMW84A-TIPO-VIA

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW84I-S-NOCOR OR IND-1 GREATER 04
              MOVE DCOMW84I-S-NOME-AVALISTA(IND-1)   TO
                                         DCOMW84A-NOME-AVAL(IND-1)
              MOVE DCOMW84I-S-CNPJ-AVALISTA(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW84I-S-FILIAL-AVALISTA(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW84I-S-CTRL-AVALISTA(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER      TO DCOMW84A-CPFPJ-AVAL(IND-1)
      *
              MOVE DCOMW84I-S-NOME-TESTEMUNHA(IND-1)   TO
                                         DCOMW84A-NOME-TEST(IND-1)
              MOVE DCOMW84I-S-CNPJ-TESTEMUNHA(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW84I-S-FILIAL-TESTEMUNHA(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW84I-S-CTRL-TESTEMUNHA(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER     TO  DCOMW84A-CPFPJ-TEST(IND-1)
      *
           END-PERFORM.

           MOVE WRK-DCOMW84A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                 OF WRK-DCOMW84A
                                           TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC003'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-BUSCAR-DADOS-FORM-W85A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ACESSAR-DCOM385A

           PERFORM 2520-ALIMENTAR-AREA-IMPD.


      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-ACESSAR-DCOM385A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2510-ACESSAR-DCOM385A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW85I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW85I-E-NUM-OPER
           MOVE DCOMW27I-E-TIPO-VIA    TO DCOMW85I-TIPO-VIA

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM385A)
                COMMAREA (WRK-AREA-DCOM385A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM385A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0140'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM385A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0150'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM385A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM385A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.


      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW85A'                    TO WRK-FORMULARIO.

           MOVE  DCOMW85I-S-CAG-BCRIA         TO DCOMW85A-CAG-BCRIA
           MOVE  DCOMW85I-S-DIGITO-AGENCIA    TO DCOMW85A-DIGITO-AGEN
           MOVE  DCOMW85I-S-NOME-AGEN         TO DCOMW85A-NOME-AGEN
           MOVE  DCOMW85I-S-CCTA-BCRIA-CLI    TO DCOMW85A-CCTA-BCRIA
           MOVE  DCOMW85I-S-DIGITO-CONTA      TO DCOMW85A-DIGITO-CONTA
           MOVE  DCOMW85I-S-CCNPJ-CPF         TO DCOMW85A-CCNPJ-CPF
           MOVE  DCOMW85I-S-CFLIAL-CNPJ       TO DCOMW85A-CFLIAL-CNPJ
           MOVE  DCOMW85I-S-CCTRL-CNPJ-CPF    TO DCOMW85A-CCTRL-CNPJ-CPF
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW85A-OPERACAO
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW85A-CONTR-COD-BARR
           MOVE  DCOMW85I-S-DINIC-OPER-DESC   TO DCOMW85A-DINIC-OPER
           MOVE  DCOMW85I-S-VOPER-DESC-COML   TO WRK-9-15-V99
           MOVE  WRK-9-17                     TO DCOMW85A-VOPER-DESC
           MOVE  DCOMW85I-S-IPSSOA-DESC-COML  TO DCOMW85A-IPSSOA-DESC
           MOVE  DCOMW85I-S-VTAC-OPER-DESC    TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW85A-VTAC-OPER-DESC
           MOVE  DCOMW85I-S-VTAR-OPER         TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW85A-VTAR-OPER
           MOVE  DCOMW85I-S-VIOF-OPER-DESC    TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW85A-VIOF-OPER-DESC
           MOVE  DCOMW85I-S-VTOT-JUROS        TO WRK-9-13-V99
           MOVE  WRK-9-15                     TO DCOMW85A-VTOT-JUROS
           MOVE  DCOMW85I-S-NOCOR             TO DCOMW85A-NOCOR
           MOVE  7                            TO DCOMW85A-NUM-COD-BARR
           MOVE CALEWAAA-DT-MMAA-MMAAAA       TO DCOMW85A-MM-AAAA-BARR
           MOVE DCOMW27I-E-TIPO-VIA           TO DCOMW85A-TIPO-VIA

           MOVE  DCOMW85I-S-QTDE-TITLO   TO DCOMW85A-QTDE-TITLO

           MOVE  DCOMW85I-S-VTOT-IOF     TO DCOMW85A-VTOT-IOF

           MOVE  DCOMW85I-S-VTOT-JUR     TO DCOMW85A-VTOT-JUR

           MOVE  DCOMW85I-S-VTOT-TITLO   TO DCOMW85A-VTOT-TITLO

           MOVE  DCOMW85I-S-VTOT-LIQUIDO TO DCOMW85A-VTOT-LIQUIDO

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL
                                  IND-1 GREATER DCOMW85I-S-NOCOR OR
                                  IND-1 GREATER 50
              MOVE  DCOMW85I-S-CTITLO-COBR-BCO(IND-1) TO
                                        DCOMW85A-CTITLO-COBR-BCO(IND-1)
              MOVE  DCOMW85I-S-NOME-SACADO(IND-1) TO
                                        DCOMW85A-NOME-SACADO(IND-1)
              MOVE  DCOMW85I-S-CNPJ-SAC(IND-1)  TO
                                        DCOMW85A-CNPJ-SAC(IND-1)
              MOVE  DCOMW85I-S-FILIAL-SAC(IND-1) TO
                                        DCOMW85A-FILIAL-SAC(IND-1)
              MOVE  DCOMW85I-S-CTRL-SAC(IND-1)  TO
                                        DCOMW85A-CTRL-SAC(IND-1)
              MOVE  DCOMW85I-S-DVCTO-TITLO(IND-1) TO
                                        DCOMW85A-DVCTO-TITLO(IND-1)
              MOVE  DCOMW85I-S-VALOR-IOF(IND-1) TO
                                        DCOMW85A-VALOR-IOF(IND-1)
              MOVE  DCOMW85I-S-VALOR-JUROS(IND-1) TO
                                        DCOMW85A-VALOR-JUROS(IND-1)
              MOVE  DCOMW85I-S-VTITLO-DESC(IND-1) TO
                                        DCOMW85A-VTITLO-DESC(IND-1)
              MOVE  DCOMW85I-S-VALOR-LIQUIDO(IND-1) TO
                                        DCOMW85A-VALOR-LIQUIDO(IND-1)
           END-PERFORM.

           MOVE WRK-DCOMW85A               TO IMPDGLB1-CONTEUDO

           MOVE LENGTH OF WRK-DCOMW85A     TO IMPDGLB1-CONTEUDO-TAM
      *
           MOVE 'DCOMW90A'                   TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER          TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC004'                      TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                        TO DCOMW90A-SEQ.
      *
      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-BUSCAR-DADOS-FORM-W86A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ACESSAR-DCOM386A

           PERFORM 2620-ALIMENTAR-AREA-IMPD.


      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-ACESSAR-DCOM386A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2610-ACESSAR-DCOM386A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW86I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW86I-E-NUM-OPER
           MOVE DCOMW27I-E-TIPO-VIA    TO DCOMW86I-TIPO-VIA

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM386A)
                COMMAREA (WRK-AREA-DCOM386A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM386A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0160'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM386A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0170'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM386A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM386A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2620-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW86A'                 TO WRK-FORMULARIO.
           MOVE  DCOMW86I-S-CAG-BCRIA         TO DCOMW86A-CAG-BCRIA
           MOVE  DCOMW86I-S-DIGITO-AGENCIA    TO DCOMW86A-DIGITO-AGEN
           MOVE  DCOMW86I-S-NOME-AGEN         TO DCOMW86A-NOME-AGEN
           MOVE  DCOMW86I-S-CCTA-BCRIA-CLI    TO DCOMW86A-CCTA-BCRIA
           MOVE  DCOMW86I-S-DIGITO-CONTA      TO DCOMW86A-DIGITO-CONTA
           MOVE  DCOMW86I-S-CCNPJ-CPF         TO DCOMW86A-CCNPJ-CPF
           MOVE  DCOMW86I-S-CFLIAL-CNPJ       TO DCOMW86A-CFLIAL-CNPJ
           MOVE  DCOMW86I-S-CCTRL-CNPJ-CPF    TO DCOMW86A-CCTRL-CNPJ-CPF
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW86A-OPERACAO
           MOVE  DCOMW86I-S-DINIC-OPER-DESC   TO DCOMW86A-DINIC-OPER
           MOVE  DCOMW86I-S-VOPER-DESC-COML   TO WRK-9-15-V99
           MOVE  WRK-9-17                     TO DCOMW86A-VOPER-DESC
           MOVE  DCOMW86I-S-IPSSOA-DESC-COML  TO DCOMW86A-IPSSOA-DESC
           MOVE  DCOMW27I-E-NUM-OPER          TO DCOMW86A-CONTR-COD-BARR
           MOVE  7                            TO DCOMW86A-NUM-COD-BARR
           MOVE  CALEWAAA-DT-MMAA-MMAAAA      TO DCOMW86A-MM-AAAA-BARR
           MOVE  DCOMW27I-E-TIPO-VIA          TO DCOMW86A-TIPO-VIA
           MOVE  DCOMW86I-S-NOCOR             TO DCOMW86A-NOCOR

           MOVE  DCOMW86I-S-VTOT-IOF-CHQ TO  WRK-9-9-V9-6
           MOVE  WRK-9-15-4      TO    DCOMW86A-VTOT-IOF-CHQ
           MOVE  DCOMW86I-S-VTOT-JUR-CHQ TO  WRK-9-9-V9-6
           MOVE  WRK-9-15-4      TO    DCOMW86A-VTOT-JUR-CHQ
           MOVE  DCOMW86I-S-VTOT-LIQ-CHQ TO WRK-9-15-V99
           MOVE  WRK-9-17        TO    DCOMW86A-VTOT-LIQ-CHQ
           MOVE  DCOMW86I-S-VSOMATOT-CHQ TO WRK-9-15-V99
           MOVE  WRK-9-17        TO    DCOMW86A-VSOMATOT-CHQ

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW86I-S-NOCOR OR IND-1 GREATER 50
               MOVE  DCOMW86I-S-CBCO-CHQ(IND-1) TO
                                           DCOMW86A-CBCO-CHQ(IND-1)
               MOVE  DCOMW86I-S-AGEN-CHQ(IND-1) TO
                                           DCOMW86A-AGEN-CHQ(IND-1)
               MOVE  DCOMW86I-S-CTA-CHQ(IND-1)  TO
                                           DCOMW86A-CTA-CHQ(IND-1)
               MOVE  DCOMW86I-S-CCPFCNPJ-CHQ(IND-1) TO
                                           DCOMW86A-CCPFCNPJ-CHQ(IND-1)
               MOVE  DCOMW86I-S-CFILIAL-CHQ(IND-1) TO
                                           DCOMW86A-CFILIAL-CHQ(IND-1)
               MOVE  DCOMW86I-S-CCONT-CHQ(IND-1) TO
                                           DCOMW86A-CCONT-CHQ(IND-1)
               MOVE  DCOMW86I-S-NCHQ(IND-1)     TO
                                           DCOMW86A-NCHQ(IND-1)
               MOVE  DCOMW86I-S-DT-DEPOS(IND-1) TO
                                           DCOMW86A-DT-DEPOS(IND-1)
               MOVE  DCOMW86I-S-VIOF-CHQ(IND-1) TO  WRK-9-9-V9-6
               MOVE  WRK-9-15-4      TO    DCOMW86A-VIOF-CHQ(IND-1)
               MOVE  DCOMW86I-S-VJUR-CHQ(IND-1) TO  WRK-9-9-V9-6
               MOVE  WRK-9-15-4      TO    DCOMW86A-VJUR-CHQ(IND-1)
               MOVE  DCOMW86I-S-VLIQ-CHQ(IND-1) TO WRK-9-13-V99
               MOVE  WRK-9-15        TO    DCOMW86A-VLIQ-CHQ(IND-1)
               MOVE  DCOMW86I-S-VTOT-CHQ(IND-1) TO  WRK-9-15-V99
               MOVE  WRK-9-17        TO    DCOMW86A-VTOT-CHQ(IND-1)
           END-PERFORM.

           MOVE WRK-DCOMW86A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                 OF WRK-DCOMW86A
                                           TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC005'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-BUSCAR-DADOS-FORM-W87A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ACESSAR-DCOM387A

           PERFORM 2720-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2710-ACESSAR-DCOM387A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2710-ACESSAR-DCOM387A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW87I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM387A)
                COMMAREA (WRK-AREA-DCOM387A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM387A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0180'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM387A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0190'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM387A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM387A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2720-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW87A'                 TO WRK-FORMULARIO.
           MOVE  DCOMW87I-S-CAG-BCRIA      TO DCOMW87A-CAG-BCRIA
           MOVE  DCOMW87I-S-DIGITO-AGEN    TO DCOMW87A-DIGITO-AGEN
           MOVE  DCOMW87I-S-NOME-AGEN      TO DCOMW87A-NOME-AGEN
           MOVE  DCOMW87I-S-CCTA-BCRIA     TO DCOMW87A-CCTA-BCRIA
           MOVE  DCOMW87I-S-DIGITO-CONTA   TO DCOMW87A-DIGITO-CONTA
           MOVE  DCOMW87I-S-CCNPJ-CPF      TO DCOMW87A-CCNPJ-CPF
           MOVE  DCOMW87I-S-CFLIAL-CNPJ    TO DCOMW87A-CFLIAL-CNPJ
           MOVE  DCOMW87I-S-CCTRL-CNPJ-CPF TO DCOMW87A-CCTRL-CNPJ-CPF
           MOVE  DCOMW27I-E-NUM-OPER       TO DCOMW87A-OPERACAO
           MOVE  DCOMW27I-E-NUM-OPER       TO DCOMW87A-NUM-COD-BARR
           MOVE  DCOMW87I-S-DINIC-OPER     TO DCOMW87A-DINIC-OPER
           MOVE  DCOMW87I-S-VOPER-DESC     TO WRK-9-15-V99
           MOVE  WRK-9-17                  TO DCOMW87A-VOPER-DESC
           MOVE  DCOMW87I-S-IPSSOA-DESC    TO DCOMW87A-IPSSOA-DESC
           MOVE  DCOMW87I-S-ENDER          TO DCOMW87A-ENDER
           MOVE  DCOMW87I-S-ICIDADE        TO DCOMW87A-ICIDADE
           MOVE  DCOMW87I-S-CEP            TO DCOMW87A-CEP
           MOVE  DCOMW87I-S-SGLUF          TO DCOMW87A-SGLUF
           MOVE  DCOMW87I-S-NOCOR          TO DCOMW87A-NOCOR
           MOVE  7                         TO DCOMW87A-NUM-COD-BARR
           MOVE  CALEWAAA-DT-MMAA-MMAAAA   TO DCOMW87A-MM-AAAA-BARR
           MOVE DCOMW27I-E-TIPO-VIA        TO DCOMW87A-TIPO-VIA
      *
           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW87I-S-NOCOR OR IND-1 GREATER 4
               MOVE  DCOMW87I-S-ITESTEMUNHA(IND-1) TO
                                        DCOMW87A-ITESTEMUNHA(IND-1)
               MOVE  DCOMW87I-S-CRGTEST(IND-1) TO
                                        DCOMW87A-CRGTEST(IND-1)
               MOVE DCOMW87I-S-CCPFCNPJ-TEST(IND-1)   TO WRK-9-9-GENER
               MOVE DCOMW87I-S-CFILIAL-TEST(IND-1)    TO WRK-9-4-GENER
               MOVE DCOMW87I-S-CCONT-TEST(IND-1)      TO WRK-9-2-GENER
               PERFORM 3100-FORMATA-CPFPJ
               MOVE WRK-X-19-GENER       TO  DCOMW87A-CPFPJ-TEST(IND-1)
           END-PERFORM.
      *
           MOVE WRK-DCOMW87A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                 OF WRK-DCOMW87A
                                           TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC007'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-BUSCAR-DADOS-FORM-W88A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-ACESSAR-DCOM388A

           PERFORM 2820-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2810-ACESSAR-DCOM388A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2810-ACESSAR-DCOM388A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW88I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW88I-E-NUM-OPER

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM388A)
                COMMAREA (WRK-AREA-DCOM388A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM388A)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0200'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM388A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0210'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM388A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM388A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2820-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW88A'                 TO WRK-FORMULARIO
           MOVE DCOMW88I-S-NNOTA-PROMIS    TO DCOMW88A-NNOTA-PROMIS
           MOVE DCOMW88I-S-DVCTO-FNAL-OPER TO DCOMW88A-DVCTO-FNAL-OPER
           MOVE DCOMW88I-S-VALOR-NOTA-PROMIS
                                           TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW88A-VALOR-NOTA-PROMIS
           MOVE DCOMW88I-S-NOME-BANCO      TO DCOMW88A-NOME-BANCO
           MOVE DCOMW88I-S-NOME-AGENCIA    TO DCOMW88A-NOME-AGENCIA
           MOVE DCOMW88I-S-VAL-OPER-EXT    TO DCOMW88A-VAL-OPER-EXT
           MOVE DCOMW88I-S-NOME-EMINENTE   TO DCOMW88A-NOME-EMINENTE
           MOVE DCOMW88I-S-RG-EMITENTE     TO DCOMW88A-RG-EMITENTE
           MOVE DCOMW88I-S-CNPJ-EMINENTE   TO DCOMW88A-CNPJ-EMINENTE
           MOVE DCOMW88I-S-FILIAL-EMINENTE TO DCOMW88A-FILIAL-EMINENTE
           MOVE DCOMW88I-S-CTRL-EMINENTE   TO DCOMW88A-CTRL-EMINENTE
           MOVE DCOMW88I-S-END-EMINENTE    TO DCOMW88A-END-EMINENTE
           MOVE DCOMW88I-S-CCEP            TO DCOMW88A-CCEP
           MOVE DCOMW88I-S-NOCOR           TO DCOMW88A-NOCOR

           MOVE DCOMW27I-E-NUM-OPER        TO WRK-OPERACAO-AUX
           MOVE WRK-NUM-CONTR              TO DCOMW88A-CBAR-01
           MOVE  7                         TO DCOMW88A-CBAR-02
           MOVE CALEWAAA-DT-MMAA-MMAAAA    TO DCOMW88A-CBAR-03

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW88I-S-NOCOR OR IND-1 GREATER 04
              MOVE DCOMW88I-S-NOME-AVAL(IND-1)
                                       TO DCOMW88A-NOME-AVAL(IND-1)
              MOVE DCOMW88I-S-CDOCTO-ID-AVAL(IND-1)
                                       TO DCOMW88A-CDOCTO-ID-AVAL(IND-1)
              MOVE DCOMW88I-S-CNPJ-AVAL(IND-1)
                                       TO DCOMW88A-CNPJ-AVAL(IND-1)
              MOVE DCOMW88I-S-FILIAL-AVAL(IND-1)
                                       TO DCOMW88A-FILIAL-AVAL(IND-1)
              MOVE DCOMW88I-S-CTRL-AVAL(IND-1)
                                       TO DCOMW88A-CTRL-AVAL(IND-1)
              MOVE DCOMW88I-S-END-AVAL(IND-1)
                                       TO DCOMW88A-END-AVAL(IND-1)
              MOVE DCOMW88I-S-CEP-AVAL(IND-1)
                                       TO DCOMW88A-CEP-AVAL(IND-1)
              MOVE DCOMW88I-S-CNPJ-AVAL(IND-1)       TO WRK-9-9-GENER
              MOVE DCOMW88I-S-FILIAL-AVAL(IND-1)     TO WRK-9-4-GENER
              MOVE DCOMW88I-S-CTRL-AVAL(IND-1)       TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER       TO DCOMW88A-CPFPJ-AVAL(IND-1)
              MOVE DCOMW88I-S-NOME-CONJUGE(IND-1)
                                       TO DCOMW88A-NOME-CNJG(IND-1)
              MOVE DCOMW88I-S-CNPJ-CNJG(IND-1)       TO WRK-9-9-GENER
              MOVE DCOMW88I-S-FILIAL-CNJG(IND-1)     TO WRK-9-4-GENER
              MOVE DCOMW88I-S-CTRL-CNJG(IND-1)       TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER       TO DCOMW88A-CPFPJ-CNJG(IND-1)
           END-PERFORM.
      *
           MOVE WRK-DCOMW88A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                     OF WRK-DCOMW88A
                                           TO IMPDGLB1-CONTEUDO-TAM.

           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC006'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-BUSCAR-DADOS-FORM-W89A     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2910-ACESSAR-DCOM389A.

           PERFORM 2920-ALIMENTAR-AREA-IMPD.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2910-ACESSAR-DCOM389A           SECTION.
      *----------------------------------------------------------------*

           MOVE '2910-ACESSAR-DCOM389A'
                                       TO FRWKGHEA-IDEN-PARAGRAFO

           INITIALIZE                  DCOMW89I-BLOCO-SAIDA

           MOVE DCOMW27I-E-NUM-OPER    TO DCOMW89I-E-CONTRATO

           MOVE DCOMW27I-E-USUARIO     TO DCOMW89I-E-USUARIO

           EXEC CICS LINK
                PROGRAM  (WRK-DCOM389A)
                COMMAREA (WRK-AREA-DCOM389A)
                LENGTH   (LENGTH OF WRK-AREA-DCOM389A)
                NOHANDLE
           END-EXEC
      *
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0220'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

           IF DCOMW000-COD-RETORNO     OF WRK-AREA-DCOM389A
                                       NOT EQUAL ZEROS
              MOVE 12                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
              MOVE '0230'              TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
              MOVE 'DCOM0008'          TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
              MOVE WRK-DCOM389A        TO FRWKGMOD-NOME-MODULO
              MOVE DCOMW000-BLOCO-RETORNO
                                       OF WRK-AREA-DCOM389A
                                       TO FRWKGMOD-BLOCO-RETORNO
              PERFORM 9100-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2920-ALIMENTAR-AREA-IMPD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMW89A'                 TO WRK-FORMULARIO.
           MOVE  7                         TO DCOMW89A-CBAR-02
                                          OF WRK-DCOMW89A
           MOVE CALEWAAA-DT-MMAA-MMAAAA    TO DCOMW89A-CBAR-03
                                          OF WRK-DCOMW89A
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW89A-CBAR-01
           MOVE DCOMW89I-S-AGENCIA         TO DCOMW89A-AGENCIA
           MOVE DCOMW89I-S-DIGITO-AGENCIA  TO DCOMW89A-DIGITO-AG
           MOVE DCOMW89I-S-NOME-AGENCIA    TO DCOMW89A-NOME-AG
           MOVE DCOMW89I-S-CONTA-CORRENTE  TO DCOMW89A-CONTA-CORR
           MOVE DCOMW89I-S-DIGITO-CONTA    TO DCOMW89A-DIGITO-CONTA
           MOVE DCOMW89I-S-CNPJ            TO DCOMW89A-CNPJ
           MOVE DCOMW89I-S-FILIAL          TO DCOMW89A-FILIAL
           MOVE DCOMW89I-S-CTRL            TO DCOMW89A-CTRL
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW89A-NR-CONTR
           MOVE DCOMW89I-S-DATA-INICIO     TO DCOMW89A-DATA-INI
           MOVE DCOMW89I-S-VALOR-LIMITE    TO WRK-9-15-V99
           MOVE WRK-9-17                   TO DCOMW89A-VALOR-LIM

           MOVE DCOMW89I-S-VL-CONTRATO-EXTENSO
                                           TO DCOMW89A-VL-CONTR-EXT
           MOVE DCOMW89I-S-NOME-CLIENTE    TO DCOMW89A-NOME-CLIENTE
           MOVE DCOMW89I-S-ENDERECO        TO DCOMW89A-ENDERECO
           MOVE DCOMW89I-S-CIDADE          TO DCOMW89A-CIDADE
           MOVE DCOMW89I-S-ESTADO          TO DCOMW89A-ESTADO
           MOVE DCOMW89I-S-QTD-OCORR       TO DCOMW89A-NOCOR
           MOVE DCOMW27I-E-TIPO-VIA        TO DCOMW89A-TIPO-VIA

           PERFORM VARYING IND-1  FROM 1 BY 1 UNTIL IND-1 GREATER
                                  DCOMW89A-NOCOR   OR IND-1 GREATER 04
              MOVE DCOMW89I-S-NOME-FIADOR(IND-1) TO
                                             DCOMW89A-NOME-FIADOR(IND-1)
              MOVE DCOMW89I-S-CNPJ-FIADOR(IND-1)       TO WRK-9-9-GENER
              MOVE DCOMW89I-S-FILIAL-FIADOR(IND-1)     TO WRK-9-4-GENER
              MOVE DCOMW89I-S-CTRL-FIADOR(IND-1)       TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER        TO DCOMW89A-CPFPJ-FIADOR(IND-1)
              MOVE 'RG'                        TO DCOMW89A-TP-DOC(IND-1)
              MOVE DCOMW89I-S-RG-FIADOR(IND-1) TO
                                               DCOMW89A-RG-FIADOR(IND-1)
              MOVE SPACES                    TO DCOMW89A-EMISS-RG(IND-1)
              MOVE SPACES                       TO DCOMW89A-UF-RG(IND-1)
              MOVE DCOMW89I-S-ENDERECO-FIADOR(IND-1) TO
                                         DCOMW89A-ENDERECO-FIADOR(IND-1)
              MOVE DCOMW89I-S-CIDADE-FIADOR(IND-1) TO
                                           DCOMW89A-CIDADE-FIADOR(IND-1)
              MOVE DCOMW89I-S-ESTADO-FIADOR(IND-1) TO
                                           DCOMW89A-ESTADO-FIADOR(IND-1)
      *
              MOVE DCOMW89I-S-NOME-CONJUGE(IND-1) TO
                                            DCOMW89A-NOME-CONJUGE(IND-1)
              MOVE DCOMW89I-S-CNPJ-CNJG(IND-1)         TO WRK-9-9-GENER
              MOVE DCOMW89I-S-FILIAL-CNJG(IND-1)       TO WRK-9-4-GENER
              MOVE DCOMW89I-S-CTRL-CNJG(IND-1)         TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER          TO DCOMW89A-CPFPJ-CNJG(IND-1)
      *
              MOVE DCOMW89I-S-NOME-TESTEMUNHA(IND-1) TO
                                         DCOMW89A-NOME-TESTEMUNHA(IND-1)
              MOVE DCOMW89I-S-CNPJ-TESTEMUNHA(IND-1)   TO WRK-9-9-GENER
              MOVE DCOMW89I-S-FILIAL-TESTEMUNHA(IND-1) TO WRK-9-4-GENER
              MOVE DCOMW89I-S-CTRL-TESTEMUNHA(IND-1)   TO WRK-9-2-GENER
              PERFORM 3100-FORMATA-CPFPJ
              MOVE WRK-X-19-GENER      TO DCOMW89A-CPFPJ-TEST(IND-1)
           END-PERFORM.
      *
           MOVE WRK-DCOMW89A               TO IMPDGLB1-CONTEUDO
           MOVE LENGTH                 OF WRK-DCOMW89A
                                           TO IMPDGLB1-CONTEUDO-TAM.
      *
           MOVE 'DCOMW90A'                 TO DCOMW90A-CDFORMULARIO
           MOVE DCOMW27I-E-NUM-OPER        TO DCOMW90A-OPERACAO
           MOVE CALEWAAA-DT-GREGO-B-DDMMAAAA TO DCOMW90A-DATA
           MOVE 'DC008'                    TO DCOMW90A-FORM WRK-DOCTO
           MOVE ZEROS                      TO DCOMW90A-SEQ.
      *
      *----------------------------------------------------------------*
       2920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PREENCHER-IMPD             SECTION.
      *----------------------------------------------------------------*

           MOVE DICDGG02-SEQUENCE      TO IMPDGLB1-CESTRT
           MOVE 1                      TO IMPDGLB1-NVRSAO
           MOVE WRK-DCOMW90A           TO IMPDGLB1-WIDTFD
           MOVE 'DCOM'                 TO IMPDGLB1-CSISTEMA
           MOVE 1                      TO IMPDGLB1-IDIOMA
           MOVE WRK-DATA-ATU           TO WRK-DATA-PTO
           MOVE '.'                    TO WRK-DATA-PTO(3:1)
           MOVE '.'                    TO WRK-DATA-PTO(6:1)
           MOVE WRK-DATA-PTO           TO IMPDGLB1-DTGERACAO
           MOVE FRWKWAAC-CIDTFD-PERIF  TO IMPDGLB1-PERIFERICO

           MOVE WRK-DOCTO              TO IMPDGLB1-CDOCTO
           MOVE WRK-FORMULARIO         TO IMPDGLB1-FRMUL
           MOVE LENGTH                 OF WRK-AREA-IMPD1201
                                       TO IMPDGLB1-TAM-LAYOUT
           MOVE 'IMPDGLB1'             TO IMPDGLB1-COD-LAYOUT.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3100-FORMATA-CPFPJ              SECTION.
      *----------------------------------------------------------------*

           MOVE  '-'                   TO WRK-X-HIF.
           MOVE  '-'                   TO WRK-X-HIF2.
           MOVE  '/'                   TO WRK-X-BARRA.

           IF   WRK-9-9-GENER          EQUAL ZEROS
                MOVE SPACES            TO WRK-X-19-GENER
                GO TO 3100-99-FIM
           END-IF.
      *
4S2511*    IF   WRK-9-4-GENER          EQUAL ZEROS
4S2511     IF   WRK-9-4-GENER          EQUAL SPACES OR
4S2511          WRK-9-4-GENER          EQUAL LOW-VALUES
               MOVE WRK-9-9-GENER     TO WRK-9-9-CPF
               MOVE WRK-9-2-GENER     TO WRK-9-2-CPF
               MOVE WRK-X-19-CPF      TO WRK-X-19-GENER
           ELSE
               MOVE WRK-9-9-GENER     TO WRK-9-9-CNPJ
               MOVE WRK-9-4-GENER     TO WRK-9-4-CNPJ
               MOVE WRK-9-2-GENER     TO WRK-9-2-CNPJ
               MOVE WRK-X-19-CNPJ     TO WRK-X-19-GENER
           END-IF.
      *
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       6000-OBTER-ESTRUTURA            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE DICDGG02-BLOCO-RETORNO
                      DICDGG02-BLOCO-ENTRADA

           MOVE 'CONSULTA'             TO DICDGG02-OPERACAO
           MOVE SPACES                 TO DICDGG02-USUARIO
           MOVE 'DCOM'                 TO DICDGG02-CENTRO-CUSTO
           MOVE 'DCOMW90A'             TO DICDGG02-E-CODE
           MOVE 1                      TO DICDGG02-NEXT-SEQUENCEFIELD
      *
           SET  DICDGG02-SEM-DETALHE   TO TRUE
      *
           EXEC CICS
                LINK PROGRAM (WRK-DICD1002)
                COMMAREA     (WRK-AREA-DICD1002)
                LENGTH       (LENGTH OF WRK-AREA-DICD1002)
           END-EXEC
      *
           IF (EIBRESP               NOT EQUAL DFHRESP(NORMAL))
               MOVE 12                 TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0240'             TO DCOMW000-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'DCOM0010'         TO DCOMW000-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 9200-TRATAR-ERRO-CICS
           END-IF

      *
           EVALUATE DICDGG02-COD-RETORNO
               WHEN 00
               WHEN 02
                   CONTINUE
               WHEN OTHER
                   MOVE 12                  TO DCOMW000-COD-RETORNO
                                            OF DFHCOMMAREA
                   MOVE '0250'              TO DCOMW000-COD-ERRO
                                            OF DFHCOMMAREA
                   MOVE DICDGG02-COD-MENSAGEM TO DCOMW000-COD-MENSAGEM
                                            OF DFHCOMMAREA
                   MOVE WRK-DICD1002        TO FRWKGMOD-NOME-MODULO
                   MOVE DICDGG02-BLOCO-RETORNO
                                            TO FRWKGMOD-BLOCO-RETORNO
                   MOVE '6000-OBTER-ESTRUTURA' TO
                                               FRWKGHEA-IDEN-PARAGRAFO
                   PERFORM 9100-TRATAR-ERRO-MODULO
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9100-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  WRK-PROGRAMA          TO FRWKGHEA-NOME-PROGRAMA.
           SET   ERRO-MODULO           TO TRUE.
           MOVE  FRWKGMOD-TAM-LAYOUT   TO FRWKGHEA-TAM-DADOS.
           MOVE  WRK-AREA-ERRO-MOD     TO WRK-BLOCO-INFO-ERRO.

           PERFORM 9999-CHAMAR-API-ERROS.
      *
      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9200-TRATAR-ERRO-CICS           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  WRK-PROGRAMA          TO FRWKGHEA-NOME-PROGRAMA.
           SET   ERRO-CICS             TO TRUE.
           MOVE  EIBFN                 TO FRWKGCIC-EIBFN.
           MOVE  EIBRCODE              TO FRWKGCIC-EIBRCODE.
           MOVE  EIBRSRCE              TO FRWKGCIC-EIBRSRCE.
           MOVE  EIBRESP               TO FRWKGCIC-EIBRESP.
           MOVE  EIBRESP2              TO FRWKGCIC-EIBRESP2.
           MOVE  EIBTASKN              TO FRWKGCIC-EIBTASKN.
           MOVE  FRWKGCIC-TAM-LAYOUT   TO FRWKGHEA-TAM-DADOS.
           MOVE  WRK-AREA-ERRO-CICS    TO WRK-BLOCO-INFO-ERRO.
      *
           PERFORM 9999-CHAMAR-API-ERROS.
      *
      *----------------------------------------------------------------*
       9200-99-FIM.                         EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9999-CHAMAR-API-ERROS           SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
      *
           PERFORM 9000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
