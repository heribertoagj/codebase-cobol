      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM3925.
       AUTHOR.         ODIRLEI COSTA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3925                                     *
      *     PROGRAMADOR.: ODIRLEI COSTA    - SONDA PROCWORK            *
      *     ANALISTA....: ODIRLEI COSTA    - SONDA PROCWORK            *
      *     DATA........: 17/03/2011                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: MODULO IMPRESSAO CET.                        *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9YPM....: MODULO IMPRESSAO CET.                        *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA.*
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.   *
      *     DCOM4174 - CONSULTA TODOS OS DADOS DE UMA OPERACAO.        *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.:                    CHAMA:    -               *
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
           '*** DCOM3925 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       77  WRK-MODNAME                 PIC  X(008)         VALUE SPACES.
       77  WRK-CONTPARM                PIC  9(005) COMP    VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BUSCA-MOVIMENTO         PIC  X(001)         VALUE SPACES.
       77  WRK-MSG-AUX                 PIC  X(079)         VALUE SPACES.

       01  WRK-BCOAGECTA-AUX.
           05  WRK-BANCO-AUX           PIC  9(03)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-AGENC-AUX           PIC  9(05)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-CONTA-AUX           PIC  9(13)          VALUE ZEROS.

VANS   01  WRK-PERC-AUX.
VANS       05  WRK-PERC-AUX-N          PIC  ZZ9,99         VALUE ZEROS.
VANS       05  WRK-PERC-AUX-SIMB       PIC  X(01)          VALUE '%'.
VANS
       01  WRK-COD-CET.
VANS       05  WRK-COD-CET-N           PIC  ZZZZ9,99       VALUE ZEROS.

       01  WRK-VALOR-TELA.
VANS       05  WRK-VALOR-TELA-N        PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

VANS   01  WRK-PERC                PIC  9(015)V9(05) COMP-3 VALUE ZEROS.
VANS   01  WRK-PERC-R              PIC  9(003)V9(02) COMP-3 VALUE ZEROS.
VANS
VANS   01  WRK-VALOR-LIQUI            PIC  9(015)V99 COMP-3 VALUE ZEROS.
VANS   01  WRK-VALOR-LIBER            PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-IOF              PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TAC-TITULOS      PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TAC-OPERACAO     PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TOT-TARIFAS      PIC  9(015)V99 COMP-3 VALUE ZEROS.
       01  WRK-VALOR-TOTAL            PIC  9(015)V99 COMP-3 VALUE ZEROS.

VANS   01  WRK-VALOR-DESCOMP          PIC  9(015)V99        VALUE ZEROS.
VANS   01  FILLER                     REDEFINES WRK-VALOR-DESCOMP.
VANS       05  FILLER                 PIC  9(006).
VANS       05  WRK-VALOR-DESCOMP-R    PIC  9(009)V99.
VANS
VANS   01  WRK-COD-CET-DIMINUI        PIC  9(008)V9(007)    VALUE ZEROS.
VANS   01  FILLER                     REDEFINES WRK-COD-CET-DIMINUI.
VANS       05  FILLER                 PIC  9(003).
VANS       05  WRK-COD-CET-DIMINUI-R  PIC  9(005)V99.
VANS       05  FILLER                 PIC  9(005).
VANS
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOM9YPM  - SEGM00 ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-9YPM.
           05 WRK-OUT-LL-9YPM             PIC S9(004) COMP VALUE +0000.
           05 WRK-OUT-ZZ-9YPM             PIC S9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-9YPM.
              10 WRK-OUT-NOMECL-9YPM       PIC  X(040)     VALUE SPACES.
              10 WRK-OUT-CNPJCPF-9YPM      PIC  X(019)     VALUE SPACES.
              10 WRK-OUT-BCAGCT-9YPM       PIC  X(023)     VALUE SPACES.
              10 WRK-OUT-CODPRD-9YPM       PIC  9(003)     VALUE ZEROS.
              10 WRK-OUT-DESPRD-9YPM       PIC  X(012)     VALUE SPACES.
              10 WRK-OUT-CODSUB-9YPM       PIC  9(003)     VALUE ZEROS.
              10 WRK-OUT-DESSUB-9YPM       PIC  X(012)     VALUE SPACES.
              10 WRK-OUT-OPERAC-9YPM       PIC  9(013)     VALUE ZEROS.
              10 FILLER REDEFINES          WRK-OUT-OPERAC-9YPM.
                 15 WRK-OUT-DANO-9YPM      PIC  9(004).
                 15 WRK-OUT-NSEQ-9YPM      PIC  9(009).
              10 WRK-OUT-SITUAC-9YPM       PIC  X(015)     VALUE SPACES.
              10 WRK-OUT-DESIDE-9YPM       PIC  X(013)     VALUE SPACES.
              10 WRK-OUT-NUMERO-9YPM.
                 15 WRK-OUT-NUMERO-9YPM-N  PIC  9(009)     VALUE ZEROS.
              10 WRK-OUT-SIT3-9YPM         PIC  X(013)     VALUE SPACES.
              10 WRK-OUT-SIT4-9YPM.
                15 WRK-OUT-SIT4-9YPM-N     PIC  9(009)     VALUE ZEROS.
VANS          10 WRK-OUT-VLRLIQU-9YPM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-VLRLIBE-9YPM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERLIBE-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TRIBUTOS-9YPM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTRIB-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-SEGUROS-9YPM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERSEGU-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TARIFAS-9YPM      PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTARI-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-PGTOSERV-9YPM     PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERPGTO-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-REGISTROS-9YPM    PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERREGI-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-TOTAL-9YPM        PIC  X(014)     VALUE SPACES.
VANS          10 WRK-OUT-PERTOTA-9YPM      PIC  X(007)     VALUE SPACES.
VANS          10 WRK-OUT-CETAM-9YPM        PIC  X(008)     VALUE SPACES.
VANS          10 WRK-OUT-CETAA-9YPM        PIC  X(008)     VALUE SPACES.
           05 WRK-OUT-MENSA-9YPM           PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(058)         VALUE
           '*** AREA DA TELA DCOM9YPM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOM9YPM.
           05  WRK-660-LL-AREA-9YPM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSAG-9YPM  PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-NOMECL-9YPM     PIC  9(004) COMP    VALUE 0040.
           05  WRK-660-CNPJCPF-9YPM    PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BCAGCT-9YPM     PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-CODPRD-9YPM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESPRD-9YPM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODSUB-9YPM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-DESSUB-9YPM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-9YPM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-9YPM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-DESIDE-9YPM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-NUMERO-9YPM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-SIT3-9YMT       PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SIT4-9YPM       PIC  9(004) COMP    VALUE 0009.
VANS       05  WRK-660-VLRLIQU-9YPM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-VLRLIBE-9YPM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERLIBE-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TRIBUTOS-9YPM   PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTRIB-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-SEGUROS-9YPM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERSEGU-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TARIFAS-9YPM    PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTARI-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-PGTOSERV-9YPM   PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERPGTO-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-REGISTROS-9YPM  PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERREGI-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-TOTAL-9YPM      PIC  9(004) COMP    VALUE 0014.
VANS       05  WRK-660-PERTOTA-9YPM    PIC  9(004) COMP    VALUE 0007.
VANS       05  WRK-660-CETAM-9YPM      PIC  9(004) COMP    VALUE 0008.
VANS       05  WRK-660-CETAA-9YPM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-MENSA-9YPM      PIC  9(004) COMP    VALUE 0079.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO DCOM5575 ***'.
      *----------------------------------------------------------------*

       01  WRK-5575-ARGUMENTOS-ENTRADA.

       03  WRK-5575-AREA-ENVIO.
           05  WRK-5575-CPRODT           PIC  9(003)       VALUE ZEROS.
           05  WRK-5575-CSPROD-DESC-COML PIC  9(003)       VALUE ZEROS.
4S2511     05  WRK-5575-CCNPJ-CPF        PIC  X(009)       VALUE SPACES.
4S2511     05  WRK-5575-CFLIAL-CNPJ      PIC  X(004)       VALUE SPACES.
           05  WRK-5575-CCTRL-CNPJ-CPF   PIC  9(002)       VALUE ZEROS.

       01  WRK-5575-AREA-RETORNO.
           05  WRK-5575-CD-RETORNO       PIC  9(004)       VALUE ZEROS.
           05  WRK-5575-MENSAGEM         PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5577              ***'.
      *----------------------------------------------------------------*

       01  WRK-5577-ARGUMENTOS-ENTRADA.
           05 WRK-5577-DANO-OPER-DESC    PIC 9(004)        VALUE ZEROS.
           05 WRK-5577-NSEQ-OPER-DESC    PIC 9(009)        VALUE ZEROS.
           05 WRK-5577-CELMTO-DESC-COML  PIC 9(003)        VALUE ZEROS.
           05 WRK-5577-CEVNTO-DESC-COML  PIC 9(003)        VALUE ZEROS.

       01  WRK-5577-RETORNO.
           05 WRK-5577-COD-RETORNO       PIC 9(004)        VALUE ZEROS.
           05 WRK-5577-MSG-RETORNO       PIC X(079)        VALUE SPACES.
           05 WRK-5577-CET-DIA           PIC 9(08)V9(7)    VALUE ZEROS.
           05 WRK-5577-CET-MES           PIC 9(08)V9(7)    VALUE ZEROS.
           05 WRK-5577-CET-ANO           PIC 9(08)V9(7)    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5537              ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3925 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

       01  LNK-AREA-DCOM3925.
           05 LNK-CD-RETORNO           PIC  9(002).
           05 LNK-IMPRESSORA           PIC  X(008).
           05 LNK-DANO-OPER-DESC       PIC  9(004).
           05 LNK-NSEQ-OPER-DESC       PIC  9(009).
           05 LNK-TIMESTAMP            PIC  X(026).
VANS       05 LNK-OPCAO                PIC  9(001).
           05 LNK-CHAMADOR             PIC  X(008).
           05 LNK-USUARIO              PIC  X(007).
VANS       05 LNK-USUARIO-R            REDEFINES
VANS          LNK-USUARIO              PIC  9(007).
VANS       05 LNK-DEPTO                PIC  X(006).

      *================================================================*
       PROCEDURE  DIVISION             USING LNK-IO-PCB LNK-ALT-PCB
                                             LNK-AREA-DCOM3925.
      *================================================================*

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'CBLTDLI'              USING WRK-CHNG
                                             LNK-ALT-PCB
                                             LNK-IMPRESSORA.

           IF  LNK-ALT-STATUS          EQUAL 'A1'
               MOVE 2                  TO LNK-CD-RETORNO
               GOBACK
           END-IF.

           IF  LNK-ALT-STATUS          NOT EQUAL SPACES
               MOVE 'IMS'              TO ERR-TIPO-ACESSO
               MOVE LNK-IMPRESSORA     TO ERR-DBD-TAB
               MOVE WRK-CHNG           TO ERR-FUN-COMANDO
               MOVE LNK-ALT-STATUS     TO ERR-STA-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO LNK-CD-RETORNO.
           MOVE SPACES                 TO WRK-OUT-DADOS-9YPM
                                          WRK-MSG-AUX.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA MENSAGEM RECEBIDA                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-BUSCA-MOVIMENTO.

           EVALUATE LNK-CHAMADOR
               WHEN 'DCOM0305'
               WHEN 'DCOM0308'
               WHEN 'DCOM0311'
                  IF  LNK-OPCAO            EQUAL 1
                      MOVE 'N'             TO WRK-BUSCA-MOVIMENTO
                  ELSE
                      MOVE 'S'             TO WRK-BUSCA-MOVIMENTO
                  END-IF

               WHEN 'DCOM0315'
               WHEN 'DCOM0336'
               WHEN 'DCOM0337'
                  MOVE 'S'                 TO WRK-BUSCA-MOVIMENTO

           END-EVALUATE.

           MOVE LNK-DANO-OPER-DESC         TO WRK-OUT-DANO-9YPM

           MOVE LNK-NSEQ-OPER-DESC         TO WRK-OUT-NSEQ-9YPM

           INITIALIZE WRK-5575-ARGUMENTOS-ENTRADA
                      WRK-5575-AREA-RETORNO.

           IF  LNK-CHAMADOR                EQUAL 'DCOM0612' OR
               LNK-CHAMADOR                EQUAL 'DCOM0618'
               PERFORM 2110-OBTER-OPER-HIST-OFIC
           ELSE
               PERFORM 2120-OBTER-OPERACAO
           END-IF.

           IF  WRK-BUSCA-MOVIMENTO     EQUAL 'S'
               PERFORM 2130-BUSCAR-CET-MOVIMENTO
           END-IF.

           PERFORM 2140-IMPRIMIR-PAGINA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTER OPERACAO DO HISTORICO                                   *
      *----------------------------------------------------------------*
       2110-OBTER-OPER-HIST-OFIC       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5537-ENTRADA-ROTEADOR

           IF  LNK-CHAMADOR             EQUAL 'DCOM0612'
               MOVE 'O'                 TO  5537-ENT-FLAG
           ELSE
               MOVE 'H'                 TO  5537-ENT-FLAG
           END-IF.

           MOVE LNK-USUARIO-R           TO 5537-ENT-CFUNC-BDSCO.
           MOVE LNK-ALT-LTERM           TO 5537-ENT-CTERM.
           MOVE LNK-DANO-OPER-DESC      TO 5537-ENT-DANO-OPER-DESC.
           MOVE LNK-NSEQ-OPER-DESC      TO 5537-ENT-NSEQ-OPER-DESC.
           MOVE LNK-TIMESTAMP           TO 5537-ENT-HULT-ATULZ.
           MOVE 'DCOM5537'              TO WRK-MODULO.

           CALL WRK-MODULO    USING     5537-ENTRADA-ROTEADOR
                                        5537-SAIDA-ROTEADOR
                                        ERRO-AREA
                                        WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO

               WHEN '0000'
                    PERFORM 2111-MONTAR-TELA

               WHEN '0003'
               WHEN '0100'
                    CONTINUE

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
VANS                MOVE  5537-SAI-MSG-RETORNO(1:75)
VANS                                TO     ERR-TEXTO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       2111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

VANS       MOVE 5537-SAI-IPSSOA-DESC-COML(1:40) TO WRK-OUT-NOMECL-9YPM

4S2511     IF 5537-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511        5537-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511        5537-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-OUT-CNPJCPF-9YPM
4S2511     ELSE
4S2511         STRING 5537-SAI-CCNPJ-CPF(1:3)  '.'
4S2511                5537-SAI-CCNPJ-CPF(4:3)  '.'
4S2511                5537-SAI-CCNPJ-CPF(7:3)  '/'
4S2511                5537-SAI-CFLIAL-CNPJ      '-'
4S2511                5537-SAI-CCTRL-CNPJ-CPF
4S2511                                    DELIMITED BY SIZE
4S2511                                    INTO WRK-OUT-CNPJCPF-9YPM
4S2511     END-IF
           MOVE 5537-SAI-CBCO           TO WRK-BANCO-AUX.
           MOVE 5537-SAI-CAG-BCRIA      TO WRK-AGENC-AUX.
           MOVE 5537-SAI-CCTA-BCRIA-CLI TO WRK-CONTA-AUX.
           MOVE WRK-BCOAGECTA-AUX       TO WRK-OUT-BCAGCT-9YPM.

           MOVE 5537-SAI-CPRODT         TO WRK-OUT-CODPRD-9YPM

           MOVE 5537-SAI-IABREV-PRODT   TO WRK-OUT-DESPRD-9YPM

           MOVE 5537-SAI-CSPROD-DESC-COML TO WRK-OUT-CODSUB-9YPM

           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DESSUB-9YPM

           MOVE 5537-SAI-IRSUMO-SIT-DESC  TO WRK-OUT-SITUAC-9YPM


VANS***    SE  5537-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
VANS***        SE  5537-SAI-CELMTO-DESC-COML  EQUAL 8
VANS***            MOVER 'CONV GERAL..:'       TO WRK-OUT-SIT3-9YPM
VANS***        SENAO
VANS***            MOVER 'CONV CLIENTE:'       TO WRK-OUT-SIT3-9YPM
VANS***        FIM-SE
VANS***        MOVER 5537-SAI-CCONTR-CONVE-DESC
VANS***                                   TO WRK-OUT-SIT4-9YPM-N
VANS***    SENAO
VANS***        SE  5537-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
VANS***            MOVER 'CONTR LIMITE:'   TO WRK-OUT-SIT3-9YPM
VANS***            MOVER 5537-SAI-CCONTR-LIM-DESC
VANS***                                   TO WRK-OUT-SIT4-9YPM-N
VANS***        FIM-SE
VANS***    FIM-SE.
VANS***
VANS       MOVE 5537-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIBER.
VANS       MOVE WRK-VALOR-LIBER            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIBE-9YPM.
VANS
           MOVE 5537-SAI-VIOF-OPER-DESC    TO WRK-VALOR-IOF.
VANS       MOVE WRK-VALOR-IOF              TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TRIBUTOS-9YPM.

           MOVE 5537-SAI-VTARIF-REG-TITLO  TO WRK-VALOR-TAC-TITULOS.
           MOVE 5537-SAI-VTAC-OPER-DESC    TO WRK-VALOR-TAC-OPERACAO.

           COMPUTE WRK-VALOR-TOT-TARIFAS   = WRK-VALOR-TAC-TITULOS
                                           + WRK-VALOR-TAC-OPERACAO.

VANS       MOVE WRK-VALOR-TOT-TARIFAS      TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TARIFAS-9YPM.

           COMPUTE WRK-VALOR-TOTAL         = WRK-VALOR-TOT-TARIFAS
                                           + WRK-VALOR-IOF.

VANS       MOVE WRK-VALOR-TOTAL            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TOTAL-9YPM.

VANS       MOVE 5537-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIQUI.
VANS       COMPUTE WRK-VALOR-LIQUI         = WRK-VALOR-LIQUI
VANS                                       + WRK-VALOR-TOTAL.
VANS       MOVE WRK-VALOR-LIQUI            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIQU-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-LIBER
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERLIBE-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-IOF
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTRIB-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOT-TARIFAS
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTARI-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOTAL
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTOTA-9YPM.
VANS
           MOVE ZEROS                      TO WRK-VALOR-TELA-N.
VANS       MOVE ZEROS                      TO WRK-PERC-AUX-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-SEGUROS-9YPM
                                              WRK-OUT-PGTOSERV-9YPM
                                              WRK-OUT-REGISTROS-9YPM.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERSEGU-9YPM
VANS                                          WRK-OUT-PERPGTO-9YPM
VANS                                          WRK-OUT-PERREGI-9YPM.

           MOVE 5537-SAI-CET-AM            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAM-9YPM.
           MOVE 5537-SAI-CET-AA            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAA-9YPM.

           MOVE 5537-SAI-CPRODT         TO WRK-5575-CPRODT.
           MOVE 5537-SAI-CSPROD-DESC-COML
                                        TO WRK-5575-CSPROD-DESC-COML.
           MOVE 5537-SAI-CCNPJ-CPF      TO WRK-5575-CCNPJ-CPF.
           MOVE 5537-SAI-CFLIAL-CNPJ    TO WRK-5575-CFLIAL-CNPJ.
           MOVE 5537-SAI-CCTRL-CNPJ-CPF TO WRK-5575-CCTRL-CNPJ-CPF.

           PERFORM 2111-10-VERIFICAR-CET.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA A EXIBICAO DO VALOR DO CET                        *
      *----------------------------------------------------------------*
       2111-10-VERIFICAR-CET           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM5575'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5575-ARGUMENTOS-ENTRADA
                                             WRK-5575-AREA-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5575-CD-RETORNO     NOT EQUAL ZEROS
               MOVE WRK-5575-MENSAGEM  TO WRK-OUT-MENSA-9YPM
           END-IF.

      *----------------------------------------------------------------*
       2111-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CONSULTA DADOS DA OPERACAO                                 *
      *----------------------------------------------------------------*
       2120-OBTER-OPERACAO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR.

           MOVE LNK-DANO-OPER-DESC     TO 4174-ENT-DANO-OPER-DESC.
           MOVE LNK-NSEQ-OPER-DESC     TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                    PERFORM 2121-FORMATAR-TELA-INICIAL

               WHEN '0099'
                    MOVE 'DB2'                TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'               TO ERR-TIPO-ACESSO
VANS                MOVE 4174-SAI-MSG-RETORNO(1:75)
VANS                                          TO ERR-TEXTO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATA TELA INICIAL COM DADOS RETORNADOS DE DCOM4174      *
      *----------------------------------------------------------------*
       2121-FORMATAR-TELA-INICIAL      SECTION.
      *----------------------------------------------------------------*

VANS       MOVE 4174-SAI-IPSSOA-DESC-COML(1:40) TO WRK-OUT-NOMECL-9YPM.

4S2511     IF 4174-SAI-CCNPJ-CPF    EQUAL SPACES OR
4S2511        4174-SAI-CCNPJ-CPF    EQUAL LOW-VALUES OR
4S2511        4174-SAI-CFLIAL-CNPJ  EQUAL SPACES OR
4S2511        4174-SAI-CFLIAL-CNPJ  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-OUT-CNPJCPF-9YPM
4S2511     ELSE
4S2511         STRING 4174-SAI-CCNPJ-CPF(1:3)
4S2511         '.'    4174-SAI-CCNPJ-CPF(4:3)
4S2511         '.'    4174-SAI-CCNPJ-CPF(7:3)
4S2511         '/'    4174-SAI-CFLIAL-CNPJ
4S2511         '-'    4174-SAI-CCTRL-CNPJ-CPF
4S2511         DELIMITED BY SIZE INTO WRK-OUT-CNPJCPF-9YPM
4S2511     END-IF

           MOVE 4174-SAI-CBCO                 TO WRK-BANCO-AUX.
           MOVE 4174-SAI-CAG-BCRIA            TO WRK-AGENC-AUX.
           MOVE 4174-SAI-CCTA-BCRIA-CLI       TO WRK-CONTA-AUX.
           MOVE WRK-BCOAGECTA-AUX             TO WRK-OUT-BCAGCT-9YPM.
           MOVE 4174-SAI-CPRODT               TO WRK-OUT-CODPRD-9YPM.

           MOVE 4174-SAI-IABREV-PRODT         TO WRK-OUT-DESPRD-9YPM.

           MOVE 4174-SAI-CSPROD-DESC-COML     TO WRK-OUT-CODSUB-9YPM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC    TO WRK-OUT-DESSUB-9YPM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC      TO WRK-OUT-SITUAC-9YPM.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 08
              MOVE 'CONV. GERAL.:'            TO WRK-OUT-DESIDE-9YPM
              MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-OUT-NUMERO-9YPM-N
           END-IF.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 09
              MOVE 'CONV.CLIENTE:'            TO WRK-OUT-DESIDE-9YPM
              MOVE 4174-SAI-CCONTR-CONVE-DESC TO WRK-OUT-NUMERO-9YPM-N
           END-IF.

           IF 4174-SAI-CELMTO-DESC-COML       EQUAL 10
              MOVE 'CONTR.LIMITE:'            TO WRK-OUT-DESIDE-9YPM
              MOVE 4174-SAI-CCONTR-LIM-DESC   TO WRK-OUT-NUMERO-9YPM-N
           END-IF.

           IF  4174-SAI-NSMULA-OPER-DESC      GREATER ZEROS
               IF  WRK-OUT-DESIDE-9YPM        EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'       TO WRK-OUT-DESIDE-9YPM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                              TO WRK-OUT-NUMERO-9YPM-N
VANS***        SENAO
VANS***            MOVER 'SIMULACAO...:'       TO WRK-OUT-SIT3-9YPM
VANS***            MOVER 4174-SAI-NSMULA-OPER-DESC
VANS***                                       TO WRK-OUT-SIT4-9YPM-N
               END-IF
           END-IF.

VANS       MOVE 4174-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIBER.
VANS       MOVE WRK-VALOR-LIBER            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIBE-9YPM.
VANS
           MOVE 4174-SAI-VIOF-OPER-DESC    TO WRK-VALOR-IOF.
VANS       MOVE WRK-VALOR-IOF              TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TRIBUTOS-9YPM.

           MOVE 4174-SAI-VTARIF-REG-TITLO  TO WRK-VALOR-TAC-TITULOS.
           MOVE 4174-SAI-VTAC-OPER-DESC    TO WRK-VALOR-TAC-OPERACAO.

           COMPUTE WRK-VALOR-TOT-TARIFAS   = WRK-VALOR-TAC-TITULOS
                                           + WRK-VALOR-TAC-OPERACAO.

VANS       MOVE WRK-VALOR-TOT-TARIFAS      TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TARIFAS-9YPM.

           COMPUTE WRK-VALOR-TOTAL         = WRK-VALOR-TOT-TARIFAS
                                           + WRK-VALOR-IOF.

VANS       MOVE WRK-VALOR-TOTAL            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-TOTAL-9YPM.

VANS       MOVE 4174-SAI-VLIQ-OPER-DESC    TO WRK-VALOR-LIQUI.
VANS       COMPUTE WRK-VALOR-LIQUI         = WRK-VALOR-LIQUI
VANS                                       + WRK-VALOR-TOTAL.
VANS       MOVE WRK-VALOR-LIQUI            TO WRK-VALOR-DESCOMP.
VANS       MOVE WRK-VALOR-DESCOMP-R        TO WRK-VALOR-TELA-N.
VANS       MOVE WRK-VALOR-TELA             TO WRK-OUT-VLRLIQU-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-LIBER
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERLIBE-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-IOF
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTRIB-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOT-TARIFAS
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTARI-9YPM.
VANS
VANS       IF  WRK-VALOR-LIQUI             GREATER ZEROS
VANS           COMPUTE WRK-PERC            = (WRK-VALOR-TOTAL
VANS                                       / WRK-VALOR-LIQUI) * 100
VANS       ELSE
VANS           MOVE ZEROS                  TO WRK-PERC
VANS       END-IF.
VANS       MOVE ZEROS                      TO WRK-PERC-R.
VANS       COMPUTE WRK-PERC-R ROUNDED      = WRK-PERC-R
VANS                                       + WRK-PERC.
VANS       MOVE WRK-PERC-R                 TO WRK-PERC-AUX-N.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERTOTA-9YPM.
VANS
           MOVE ZEROS                      TO WRK-VALOR-TELA-N.
VANS       MOVE ZEROS                      TO WRK-PERC-AUX-N.
           MOVE WRK-VALOR-TELA             TO WRK-OUT-SEGUROS-9YPM
                                              WRK-OUT-PGTOSERV-9YPM
                                              WRK-OUT-REGISTROS-9YPM.
VANS       MOVE WRK-PERC-AUX               TO WRK-OUT-PERSEGU-9YPM
VANS                                          WRK-OUT-PERPGTO-9YPM
VANS                                          WRK-OUT-PERREGI-9YPM.

           MOVE 4174-SAI-CET-AM            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAM-9YPM.
           MOVE 4174-SAI-CET-AA            TO WRK-COD-CET-N.
           MOVE WRK-COD-CET                TO WRK-OUT-CETAA-9YPM.

           MOVE 4174-SAI-CPRODT         TO WRK-5575-CPRODT.
           MOVE 4174-SAI-CSPROD-DESC-COML
                                        TO WRK-5575-CSPROD-DESC-COML.
           MOVE 4174-SAI-CCNPJ-CPF      TO WRK-5575-CCNPJ-CPF.
           MOVE 4174-SAI-CFLIAL-CNPJ    TO WRK-5575-CFLIAL-CNPJ.
           MOVE 4174-SAI-CCTRL-CNPJ-CPF TO WRK-5575-CCTRL-CNPJ-CPF.

           PERFORM 2111-10-VERIFICAR-CET.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     BUSCAR O VALOR DO CET CALCULADO NAS BASES DO MOVIMENTO C3  *
      *----------------------------------------------------------------*
       2130-BUSCAR-CET-MOVIMENTO       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5577-ARGUMENTOS-ENTRADA
                      WRK-5577-RETORNO.

           MOVE LNK-DANO-OPER-DESC   TO WRK-5577-DANO-OPER-DESC.
           MOVE LNK-NSEQ-OPER-DESC   TO WRK-5577-NSEQ-OPER-DESC.

           IF  LNK-CHAMADOR            EQUAL 'DCOM0315'
                                          OR 'DCOM0336'
                                          OR 'DCOM0337'
VANS           IF  LNK-OPCAO           EQUAL  3
      * -->   FORMALIZACAO DE ADITAMENTO DE PARCELA
                   MOVE 7              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 25             TO WRK-5577-CEVNTO-DESC-COML
               ELSE
      * -->   FORMALIZACAO DE PRORROGACAO DE PARCELA
                   MOVE 7              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 08             TO WRK-5577-CEVNTO-DESC-COML
               END-IF
           ELSE
VANS           IF  LNK-OPCAO           NOT EQUAL 1
                   MOVE 6              TO WRK-5577-CELMTO-DESC-COML
                   MOVE 26             TO WRK-5577-CEVNTO-DESC-COML
               END-IF
           END-IF.

           MOVE 'DCOM5577'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5577-ARGUMENTOS-ENTRADA
                                             WRK-5577-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5577-COD-RETORNO
               WHEN ZEROS
VANS                MOVE WRK-5577-CET-MES   TO WRK-COD-CET-DIMINUI
VANS                MOVE WRK-COD-CET-DIMINUI-R
VANS                                        TO WRK-COD-CET-N
                    MOVE WRK-COD-CET        TO WRK-OUT-CETAM-9YPM
VANS                MOVE WRK-5577-CET-ANO   TO WRK-COD-CET-DIMINUI
VANS                MOVE WRK-COD-CET-DIMINUI-R
VANS                                        TO WRK-COD-CET-N
                    MOVE WRK-COD-CET        TO WRK-OUT-CETAA-9YPM

               WHEN 99
                    MOVE 'DB2'                TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 'APL'                TO ERR-TIPO-ACESSO
VANS                MOVE WRK-5577-MSG-RETORNO(1:75)
VANS                                          TO ERR-TEXTO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     IMPRIMIR A PAGINA SOLICITADA VIA PF4                       *
      *----------------------------------------------------------------*
       2140-IMPRIMIR-PAGINA            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2141-PROCESSAR-BRAD0660.

           PERFORM 2142-INSERIR-SEGTOS.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DA BRAD0660 - INSERCAO DA TELA DCOM9YPM      *
      *----------------------------------------------------------------*
       2141-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-9YPM
                                       TO WRK-OUT-LL-9YPM
                                          WRK-660-LL-MENSAG-9YPM.
           MOVE LENGTH                 OF WRK-660-DCOM9YPM
                                       TO WRK-660-LL-AREA-9YPM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-9YPM
                                             WRK-660-DCOM9YPM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INSERE OS SEGMENTOS DA IMPRESSAO                           *
      *----------------------------------------------------------------*
       2142-INSERIR-SEGTOS             SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM9YPM'             TO WRK-MODNAME.
           MOVE 0004                   TO WRK-CONTPARM.

           CALL 'CBLTDLI'              USING WRK-CONTPARM
                                             WRK-ISRT
                                             LNK-ALT-PCB
                                             WRK-OUTPUT-9YPM
                                             WRK-MODNAME.

           IF  LNK-ALT-STATUS          NOT EQUAL SPACES
               MOVE 'IMS'              TO ERR-TIPO-ACESSO
               MOVE WRK-MODNAME        TO ERR-DBD-TAB
               MOVE WRK-ISRT           TO ERR-FUN-COMANDO
               MOVE LNK-ALT-STATUS     TO ERR-STA-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2142-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DE ERRO                                      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3925'         TO ERR-PGM
           ELSE
               MOVE 'DCOM3924'         TO ERR-PGM
               MOVE 'DCOM3925'         TO ERR-MODULO
           END-IF.

           MOVE LNK-USUARIO            TO ERR-COD-USER.
           MOVE LNK-DEPTO              TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           MOVE 4                      TO LNK-CD-RETORNO.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
