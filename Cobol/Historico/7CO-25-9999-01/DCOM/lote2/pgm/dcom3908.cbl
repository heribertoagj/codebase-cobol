      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM3908.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3908                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: MARCELINO - ALTRAN.                          *
      *     DATA........: 06/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA PARA SELECAO                           *
      *                   SELECAO DE CONVENIO GERAL                    *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9HTM....: LISTA PARA SELECAO                           *
      *                   SELECAO DE CONVENIO GERAL                    *
      *----------------------------------------------------------------*

      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM3906                                     *
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
       77  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3908 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC S9(09) COMP     VALUE ZEROS.
       77  WIND-SEL                    PIC S9(09) COMP     VALUE ZEROS.
       77  WCT-SELEC                   PIC S9(09) COMP     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                     PIC S9(08) COMP     VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(08) COMP     VALUE +49353.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM3906 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004)  COMP   VALUE +343.
           05  COMU-ZZ                 PIC S9(004)  COMP   VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TPOPCAO        PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
               10  COMU-CHAMADO        PIC  X(008)         VALUE SPACES.
               10  COMU-NOME           PIC  X(040)         VALUE ZEROS.
               10  COMU-CPFCNPJ        PIC  X(017)         VALUE SPACES.
               10  COMU-BANCO-X.
                   15  COMU-BANCO      PIC  9(003)         VALUE ZEROS.

               10  COMU-AGENCIA.
                   15  COMU-COD-AGE    PIC  9(005)         VALUE ZEROS.
                   15  FILLER          PIC  X(021)         VALUE SPACES.
               10  COMU-CONTA-X.
                   15  COMU-CONTA      PIC  9(013)         VALUE ZEROS.
               10  COMU-COD-PROD-X.
                   15  COMU-COD-PROD   PIC  9(003)         VALUE ZEROS.
               10  COMU-COD-SUBP-X.
                   15  COMU-COD-SUBP   PIC  9(003)         VALUE ZEROS.
               10  COMU-MENSAGEM       PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-103.
           05  COMU-LL-103             PIC S9(004)  COMP   VALUE +241.
           05  COMU-ZZ-103             PIC S9(004)  COMP   VALUE ZEROS.
           05  COMU-TRANCODE-103.
               10  COMU-TRANSACAO-103  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.

               10  COMU-PFK-103        PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-103         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-103        PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-103.
               10  COMU-OPCAO-103      PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP-103  PIC  X(026)         VALUE SPACES.
               10  COMU-COD-OPER-103-X.
                   15  COMU-COD-OPER-103
                                       PIC  9(009)         VALUE ZEROS.
               10  COMU-TPOPER-103     PIC  X(002)         VALUE SPACES.
               10  COMU-MENSA-103      PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4320 ***'.
      *----------------------------------------------------------------*
       01  WRK-4320E-ENTRADA.
           03  WRK-4320E-COD-RETORNO   PIC  X(04).
           03  WRK-4320E-MSG-RETORNO   PIC  X(79).
           03  WRK-4320E-RESTART       PIC  9(05).
           03  WRK-4320E-FLAG          PIC  X(01).

           03  WRK-4320E-START-COUNT   PIC  X(01).
           03  WRK-4320E-CFUNC-BDSCO   PIC  9(09).
           03  WRK-4320E-CTERM         PIC  X(08).
           03  WRK-4320E-CPROD         PIC  9(03).
           03  WRK-4320E-CSPROD-DESC-COML
                                       PIC  9(03).
           03  WRK-4320E-BANCO         PIC  9(03).
           03  WRK-4320E-DEPDC         PIC  9(05).
           03  WRK-4320E-CONTA         PIC  9(07).

       01  WRK-4320S-SAIDA.
           03  WRK-4320S-HEADER.
               05  WRK-4320S-COD-RETORNO
                                       PIC  X(04).
               05  WRK-4320S-MSG-RETORNO
                                       PIC  X(79).
               05  WRK-4320S-RESTART   PIC  9(05).
               05  WRK-4320S-FLAG      PIC  X(01).
               05  WRK-4320S-QTDE-RETORNADA
                                       PIC  9(03).
               05  WRK-4320S-COUNT     PIC  9(09).


           03  WRK-4320S-DADOS.
               05  WRK-4320S-TABELA-SAIDA  OCCURS  30  TIMES.
                   07 WRK-4320S-CCONVE-GRAL-DESC
                                       PIC  9(09).
                   07 WRK-4320S-CVRSAO-CONVE-GRAL
                                       PIC  9(03).
                   07 WRK-4320S-ICONVE-GRAL-DESC
                                       PIC  X(40).
                   07 WRK-4320S-DINIC-VGCIA-CONVE
                                       PIC  X(10).
                   07 WRK-4320S-DVCTO-CONVE-DESC
                                       PIC  X(10).
           03  WRK-4320S-CONSISTENCIA.
               05  WRK-4320S-TAMANHO-ERRO
                                       PIC  9(01).
               05  WRK-4320S-TABELA-ERROS  OCCURS 02  TIMES.
                   07  WRK-4320S-CAMPOS-ERRO
                                       PIC  9(01).

      *---------------------------------------------------------------*

       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO           PIC  9(06)          VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9HTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-9HTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-9HTM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-9HTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-9HTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-9HTM.
               10  WRK-INP-TMSTAMP-9HTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CHAMADO-9HTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-CDPROD-9HTM-X.
                   15  WRK-INP-CDPROD-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-CDSUBP-9HTM-X.
                   15  WRK-INP-CDSUBP-9HTM
                                       PIC  9(03)          VALUE ZEROS.

               10  WRK-INP-TIPOOPC-9HTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-ULTPAG-9HTM PIC  X(01)          VALUE SPACES.
               10  WRK-INP-PAGINA-9HTM-X.
                   15  WRK-INP-PAGINA-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-NOMECL-9HTM PIC  X(30)          VALUE SPACES.
               10  WRK-INP-CGCCPF-9HTM.
4S2511*            15  WRK-INP-CGCCPF1-9HTM PIC  9(03)     VALUE ZEROS.
4S2511             15  WRK-INP-CGCCPF1-9HTM PIC  X(03)     VALUE SPACES.
                   15  WRK-INP-PONTO1-9HTM  PIC  X(01)     VALUE '.'.
4S2511*            15  WRK-INP-CGCCPF2-9HTM PIC  9(03)     VALUE ZEROS.
4S2511             15  WRK-INP-CGCCPF2-9HTM PIC  X(03)     VALUE SPACES.
                   15  WRK-INP-PONTO2-9HTM  PIC  X(01)     VALUE '.'.
4S2511*            15  WRK-INP-CGCCPF3-9HTM PIC  9(03)     VALUE ZEROS.
4S2511             15  WRK-INP-CGCCPF3-9HTM PIC  9(03)     VALUE SPACES.
                   15  WRK-INP-BARRA-9HTM   PIC  X(01)     VALUE '/'.
4S2511*            15  WRK-INP-FIL-9HTM     PIC  9(04)     VALUE ZEROS.
4S2511             15  WRK-INP-FIL-9HTM     PIC  9(04)     VALUE SPACES.
                   15  WRK-INP-TRACO-9HTM   PIC  X(01)     VALUE '-'.
                   15  WRK-INP-CTRL-9HTM    PIC  9(02)     VALUE ZEROS.
               10  WRK-INP-BANCO-9HTM-X.
                   15  WRK-INP-BANCO-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-AGENCIA-9HTM

                                       PIC  X(26)          VALUE SPACES.
               10  FILLER REDEFINES  WRK-INP-AGENCIA-9HTM.
                   15  WRK-INP-CODAGE-9HTM PIC  9(05).
                   15  FILLER              PIC  X(21).
               10  WRK-INP-CONTA-9HTM-X.
                   15  WRK-INP-CONTA-9HTM
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-INP-LINHAS-9HTM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-9HTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-INP-CODIGO-9HTM.
                       20  WRK-INP-CODIGO-9HTM-N
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-INP-DESCRI-9HTM
                                       PIC  X(40)          VALUE SPACES.
                   15  WRK-INP-DTINIC-9HTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-INP-DTVENC-9HTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-INP-MENSA-9HTM      PIC  X(79)          VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOM9HTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-9HTM.
           05  WRK-OUT-LL-9HTM         PIC S9(04) COMP     VALUE +1044.
           05  WRK-OUT-ZZ-9HTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-9HTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-9HTM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-9HTM.
               10  WRK-OUT-TMSTAMP-9HTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-9HTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-CDPROD-9HTM-X.
                   15  WRK-OUT-CDPROD-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-CDSUBP-9HTM-X.
                   15  WRK-OUT-CDSUBP-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-TIPOOPC-9HTM

                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-ULTPAG-9HTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-9HTM-X.
                   15  WRK-OUT-PAGINA-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-NOMECL-9HTM PIC  X(30)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-9HTM.
                   15  WRK-OUT-CGCCPF1-9HTM PIC  9(03)     VALUE ZEROS.
                   15  WRK-OUT-PONTO1-9HTM  PIC  X(01)     VALUE '.'.
                   15  WRK-OUT-CGCCPF2-9HTM PIC  9(03)     VALUE ZEROS.
                   15  WRK-OUT-PONTO2-9HTM  PIC  X(01)     VALUE '.'.
                   15  WRK-OUT-CGCCPF3-9HTM PIC  9(03)     VALUE ZEROS.
                   15  WRK-OUT-BARRA-9HTM   PIC  X(01)     VALUE '/'.
                   15  WRK-OUT-FIL-9HTM     PIC  9(04)     VALUE ZEROS.
                   15  WRK-OUT-TRACO-9HTM   PIC  X(01)     VALUE '-'.
                   15  WRK-OUT-CTRL-9HTM    PIC  9(02)     VALUE ZEROS.
               10  WRK-OUT-BANCO-9HTM-X.
                   15  WRK-OUT-BANCO-9HTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-AGENCIA-9HTM
                                       PIC  X(26)          VALUE SPACES.

               10  WRK-OUT-CONTA-9HTM-X.
                   15  WRK-OUT-CONTA-9HTM
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-LINHAS-9HTM OCCURS 10 TIMES.
                   15  WRK-OUT-SELEC-ATTR-9HTM
                                       PIC  9(004) COMP    VALUE ZEROS.
                   15  WRK-OUT-SELEC-9HTM
                                       PIC  X(01)          VALUE SPACE.
                   15  WRK-OUT-CODIGO-9HTM.
                       20  WRK-OUT-CODIGO-9HTM-N
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-OUT-DESCRI-9HTM
                                       PIC  X(40)          VALUE SPACES.
                   15  WRK-OUT-DTINIC-9HTM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-OUT-DTVENC-9HTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-OUT-MENSA-9HTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9HTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOM9HTM.
           05  WRK-660-LL-AREA-9HTM    PIC S9(004) COMP    VALUE +0134.
           05  WRK-660-LL-MENSA-9HTM   PIC S9(004) COMP    VALUE +1044.
           05  WRK-660-SENHAS-9HTM     PIC  9(004) COMP    VALUE 0037.

           05  WRK-660-COMANDO-9HTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TMSTAMP-9HTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-9HTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-CDPROD-9HTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-CDSUBP-9HTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TIPOOPC-9HTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-ULTPAG-9HTM     PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-PAGINA-9HTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NOMECL-9HTM     PIC  9(004) COMP    VALUE 0030.
           05  WRK-660-CGCCPF-9HTM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BANCO-9HTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGENCIA-9HTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-NUCONTA-9HTM    PIC  9(004) COMP    VALUE 0013.
           05  WRK-600-LISTA-9HTM      OCCURS  10.
               10 WRK-660-ESCOLH-9HTM  PIC  9(004) COMP    VALUE 2003.
               10 WRK-660-CODIGO-9HTM  PIC  9(004) COMP    VALUE 0009.
               10 WRK-660-DESCRI-9HTM  PIC  9(004) COMP    VALUE 0040.
               10 WRK-660-DTINIC-9HTM  PIC  9(004) COMP    VALUE 0010.
               10 WRK-660-DTVENC-9HTM  PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-9HTM      PIC  9(004) COMP    VALUE 0079.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3908 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*


       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.


           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.


           MOVE SPACES                 TO WRK-OUT-DADOS-9HTM
                                          WRK-OUT-SENHAS-9HTM
                                          WRK-OUT-COMANDO-9HTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM9HTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3908'

               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9HTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9HTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA.
           MOVE COMU-TIMESTAMP         TO WRK-INP-TMSTAMP-9HTM

                                          WRK-OUT-TMSTAMP-9HTM.
           MOVE COMU-TPOPCAO           TO WRK-INP-TIPOOPC-9HTM
                                          WRK-OUT-TIPOOPC-9HTM.
           MOVE COMU-CHAMADO           TO WRK-INP-CHAMADO-9HTM
                                          WRK-OUT-CHAMADO-9HTM.
           MOVE COMU-NOME              TO WRK-INP-NOMECL-9HTM
                                          WRK-OUT-NOMECL-9HTM.

           STRING COMU-CPFCNPJ(1:3) '.'
                  COMU-CPFCNPJ(4:3) '.'
                  COMU-CPFCNPJ(7:3) '/'
                  COMU-CPFCNPJ(11:4) '-'
                  COMU-CPFCNPJ(16:2)  DELIMITED BY SIZE
                                     INTO WRK-INP-CGCCPF-9HTM.
           MOVE WRK-INP-CGCCPF-9HTM    TO WRK-OUT-CGCCPF-9HTM.

           MOVE COMU-BANCO             TO WRK-INP-BANCO-9HTM
                                          WRK-OUT-BANCO-9HTM.
           MOVE COMU-AGENCIA           TO WRK-INP-AGENCIA-9HTM
                                          WRK-OUT-AGENCIA-9HTM.
           MOVE COMU-CONTA             TO WRK-INP-CONTA-9HTM

                                          WRK-OUT-CONTA-9HTM.
           MOVE COMU-COD-PROD          TO WRK-INP-CDPROD-9HTM
                                          WRK-OUT-CDPROD-9HTM.
           MOVE COMU-COD-SUBP          TO WRK-INP-CDSUBP-9HTM
                                          WRK-OUT-CDSUBP-9HTM.
           MOVE 1                      TO WRK-INP-PAGINA-9HTM
                                          WRK-OUT-PAGINA-9HTM.

           MOVE 'N'                    TO WRK-4320E-FLAG
           MOVE 'S'                    TO WRK-4320E-START-COUNT
           MOVE ZEROS                  TO WRK-4320E-RESTART
           MOVE WRK-COD-USER           TO WRK-4320E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4320E-CTERM
           MOVE COMU-COD-PROD          TO WRK-4320E-CPROD
           MOVE COMU-COD-SUBP          TO WRK-4320E-CSPROD-DESC-COML.
           MOVE COMU-BANCO             TO WRK-4320E-BANCO
           MOVE COMU-COD-AGE           TO WRK-4320E-DEPDC.
           MOVE COMU-CONTA             TO WRK-4320E-CONTA.

           MOVE 'DCOM4320'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-4320E-ENTRADA
                                             WRK-4320S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4320S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4320S-CCONVE-GRAL-DESC(11)  NOT NUMERIC) OR
                      (WRK-4320S-CCONVE-GRAL-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9HTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9HTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9HTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9HTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

               WHEN '0099'

                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0003'
                   MOVE WRK-CHNG               TO WRK-FUNCAO
                   MOVE WRK-INP-CHAMADO-9HTM   TO WRK-TELA
                   MOVE 'DCOM3908'             TO COMU-TRANSACAO
                   MOVE WRK-INP-SENHAS-9HTM    TO COMU-SENHAS
                   MOVE WRK-INP-COMANDO-9HTM   TO COMU-COMANDO
                   MOVE WRK-INP-TIPOOPC-9HTM   TO COMU-TPOPCAO
                   MOVE WRK-INP-TMSTAMP-9HTM   TO COMU-TIMESTAMP
                   MOVE WRK-INP-CHAMADO-9HTM   TO COMU-CHAMADO
                   MOVE WRK-INP-NOMECL-9HTM    TO COMU-NOME

                   MOVE WRK-INP-CGCCPF1-9HTM   TO COMU-CPFCNPJ(1:3)
                   MOVE WRK-INP-CGCCPF2-9HTM   TO COMU-CPFCNPJ(4:3)
                   MOVE WRK-INP-CGCCPF3-9HTM   TO COMU-CPFCNPJ(7:3)
                   MOVE '/'                    TO COMU-CPFCNPJ(10:1)
                   MOVE WRK-INP-FIL-9HTM       TO COMU-CPFCNPJ(11:4)
                   MOVE '-'                    TO COMU-CPFCNPJ(15:1)
                   MOVE WRK-INP-CTRL-9HTM      TO COMU-CPFCNPJ(16:2)


                   MOVE WRK-INP-BANCO-9HTM     TO COMU-BANCO
                   MOVE WRK-INP-AGENCIA-9HTM   TO COMU-AGENCIA
                   MOVE WRK-INP-CONTA-9HTM     TO COMU-CONTA
                   MOVE WRK-INP-CDPROD-9HTM    TO COMU-COD-PROD
                   MOVE WRK-INP-CDSUBP-9HTM    TO COMU-COD-SUBP
                   MOVE 'NAO EXISTE CONVENIO CADASTRADO PARA O PRODUTO E
      -                 ' SUBPRODUTO INFORMADO' TO COMU-MENSAGEM
                   MOVE COMU-AREA              TO WRK-MENSAGEM

               WHEN OTHER
                   MOVE WRK-CHNG               TO WRK-FUNCAO
                   MOVE WRK-INP-CHAMADO-9HTM   TO WRK-TELA
                   MOVE 'DCOM3908'             TO COMU-TRANSACAO
                   MOVE WRK-INP-SENHAS-9HTM    TO COMU-SENHAS
                   MOVE WRK-INP-COMANDO-9HTM   TO COMU-COMANDO
                   MOVE WRK-INP-TIPOOPC-9HTM   TO COMU-TPOPCAO
                   MOVE WRK-INP-TMSTAMP-9HTM   TO COMU-TIMESTAMP
                   MOVE WRK-INP-CHAMADO-9HTM   TO COMU-CHAMADO
                   MOVE WRK-INP-NOMECL-9HTM    TO COMU-NOME


                   MOVE WRK-INP-CGCCPF1-9HTM   TO COMU-CPFCNPJ(1:3)
                   MOVE WRK-INP-CGCCPF2-9HTM   TO COMU-CPFCNPJ(4:3)
                   MOVE WRK-INP-CGCCPF3-9HTM   TO COMU-CPFCNPJ(7:3)
                   MOVE '/'                    TO COMU-CPFCNPJ(10:1)
                   MOVE WRK-INP-FIL-9HTM       TO COMU-CPFCNPJ(11:4)
                   MOVE '-'                    TO COMU-CPFCNPJ(15:1)
                   MOVE WRK-INP-CTRL-9HTM      TO COMU-CPFCNPJ(16:2)

                   MOVE WRK-INP-BANCO-9HTM     TO COMU-BANCO
                   MOVE WRK-INP-AGENCIA-9HTM   TO COMU-AGENCIA
                   MOVE WRK-INP-CONTA-9HTM     TO COMU-CONTA
                   MOVE WRK-INP-CDPROD-9HTM    TO COMU-COD-PROD
                   MOVE WRK-INP-CDSUBP-9HTM    TO COMU-COD-SUBP
                   MOVE WRK-4320S-MSG-RETORNO  TO COMU-MENSAGEM
                   MOVE COMU-AREA              TO WRK-MENSAGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4320S-CCONVE-GRAL-DESC(WRK-IND) NOT NUMERIC) OR
              (WRK-4320S-CCONVE-GRAL-DESC(WRK-IND) EQUAL ZEROS)
               MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-9HTM(WRK-IND)
           ELSE
               MOVE WRK-4320S-CCONVE-GRAL-DESC(WRK-IND)
                                     TO WRK-OUT-CODIGO-9HTM-N(WRK-IND)
               MOVE WRK-4320S-ICONVE-GRAL-DESC(WRK-IND)
                                     TO WRK-OUT-DESCRI-9HTM(WRK-IND)
               MOVE WRK-4320S-DINIC-VGCIA-CONVE(WRK-IND)
                                     TO WRK-OUT-DTINIC-9HTM(WRK-IND)
               INSPECT WRK-OUT-DTINIC-9HTM(WRK-IND)
                                        REPLACING ALL '.' BY '/'
               MOVE WRK-4320S-DVCTO-CONVE-DESC(WRK-IND)
                                     TO WRK-OUT-DTVENC-9HTM(WRK-IND)
               INSPECT WRK-OUT-DTVENC-9HTM(WRK-IND)

                                        REPLACING ALL '.' BY '/'
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOM9HTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9HTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-9HTM.

           PERFORM 2205-DEVOLVER-TELA-FIXO.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-9HTM      EQUAL 'H' OR
                   (WRK-INP-PFK-9HTM      EQUAL '.' AND

                    WRK-INP-COMANDO-9HTM  EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-9HTM TO  WRK-OUT-MENSA-9HTM

               WHEN WRK-INP-PFK-9HTM      EQUAL '3' OR
                   (WRK-INP-PFK-9HTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9HTM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-9HTM      EQUAL '7' OR
                   (WRK-INP-PFK-9HTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9HTM  EQUAL 'PFK07')
                    PERFORM 2240-TELA-ANTERIOR

               WHEN WRK-INP-PFK-9HTM      EQUAL '8' OR
                   (WRK-INP-PFK-9HTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9HTM  EQUAL 'PFK08')
                    PERFORM 2250-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-9HTM     EQUAL '.' AND
                     WRK-INP-COMANDO-9HTM EQUAL SPACES)

                     PERFORM 2270-TRATAR-ENTER
                     PERFORM 2210-DEVOLVER-TELA-VARIA

               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)
                                       TO  WRK-OUT-MENSA-9HTM
                     PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-9HTM    TO WRK-OUT-SENHAS-9HTM

           MOVE WRK-INP-TMSTAMP-9HTM   TO WRK-OUT-TMSTAMP-9HTM
           MOVE WRK-INP-CHAMADO-9HTM   TO WRK-OUT-CHAMADO-9HTM
           MOVE WRK-INP-CDPROD-9HTM-X  TO WRK-OUT-CDPROD-9HTM-X
           MOVE WRK-INP-CDSUBP-9HTM-X  TO WRK-OUT-CDSUBP-9HTM-X
           MOVE WRK-INP-TIPOOPC-9HTM   TO WRK-OUT-TIPOOPC-9HTM
           MOVE WRK-INP-ULTPAG-9HTM    TO WRK-OUT-ULTPAG-9HTM
           MOVE WRK-INP-PAGINA-9HTM-X  TO WRK-OUT-PAGINA-9HTM-X
           MOVE WRK-INP-MENSA-9HTM     TO WRK-OUT-MENSA-9HTM

           MOVE WRK-INP-BANCO-9HTM-X   TO WRK-OUT-BANCO-9HTM-X
           MOVE WRK-INP-AGENCIA-9HTM   TO WRK-OUT-AGENCIA-9HTM
           MOVE WRK-INP-CONTA-9HTM-X   TO WRK-OUT-CONTA-9HTM-X
           MOVE WRK-INP-CGCCPF-9HTM    TO WRK-OUT-CGCCPF-9HTM
           MOVE WRK-INP-NOMECL-9HTM   TO WRK-OUT-NOMECL-9HTM.

      *----------------------------------------------------------------*
       2205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 10

               IF (WRK-INP-CODIGO-9HTM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-CODIGO-9HTM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-9HTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-9HTM(WRK-IND)
                                     TO WRK-OUT-SELEC-9HTM(WRK-IND)
                   MOVE WRK-INP-CODIGO-9HTM(WRK-IND)
                                     TO WRK-OUT-CODIGO-9HTM(WRK-IND)
                   MOVE WRK-INP-DESCRI-9HTM(WRK-IND)
                                     TO WRK-OUT-DESCRI-9HTM(WRK-IND)
                   MOVE WRK-INP-DTINIC-9HTM(WRK-IND)
                                     TO WRK-OUT-DTINIC-9HTM(WRK-IND)
                   MOVE WRK-INP-DTVENC-9HTM(WRK-IND)
                                     TO WRK-OUT-DTVENC-9HTM(WRK-IND)

               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0102                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF COMU-AREA    TO COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE WRK-INP-CHAMADO-9HTM   TO WRK-TELA
           MOVE 'DCOM3908'             TO COMU-TRANSACAO
           MOVE '3'                    TO COMU-PFK
           MOVE WRK-INP-SENHAS-9HTM    TO COMU-SENHAS
           MOVE WRK-INP-COMANDO-9HTM   TO COMU-COMANDO
           MOVE WRK-INP-TIPOOPC-9HTM   TO COMU-TPOPCAO

           MOVE WRK-INP-TMSTAMP-9HTM   TO COMU-TIMESTAMP
           MOVE WRK-INP-CHAMADO-9HTM   TO COMU-CHAMADO
           MOVE WRK-INP-NOMECL-9HTM    TO COMU-NOME

           MOVE WRK-INP-CGCCPF1-9HTM   TO COMU-CPFCNPJ(1:3)
           MOVE WRK-INP-CGCCPF2-9HTM   TO COMU-CPFCNPJ(4:3)
           MOVE WRK-INP-CGCCPF3-9HTM   TO COMU-CPFCNPJ(7:3)
           MOVE '/'                    TO COMU-CPFCNPJ(10:1)
           MOVE WRK-INP-FIL-9HTM       TO COMU-CPFCNPJ(11:4)
           MOVE '-'                    TO COMU-CPFCNPJ(15:1)
           MOVE WRK-INP-CTRL-9HTM      TO COMU-CPFCNPJ(16:2)

           MOVE WRK-INP-BANCO-9HTM     TO COMU-BANCO
           MOVE WRK-INP-AGENCIA-9HTM   TO COMU-AGENCIA
           MOVE WRK-INP-CONTA-9HTM     TO COMU-CONTA
           MOVE WRK-INP-CDPROD-9HTM    TO COMU-COD-PROD
           MOVE WRK-INP-CDSUBP-9HTM    TO COMU-COD-SUBP
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2240-TELA-ANTERIOR              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-9HTM  EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MENSA-9HTM
               GO TO 2240-99-FIM
           END-IF.


           MOVE 'N'                    TO WRK-4320E-FLAG
           MOVE 'N'                    TO WRK-4320E-START-COUNT
           MOVE WRK-COD-USER           TO WRK-4320E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4320E-CTERM
           MOVE WRK-INP-CDPROD-9HTM    TO WRK-4320E-CPROD
           MOVE WRK-INP-CDSUBP-9HTM    TO WRK-4320E-CSPROD-DESC-COML.

           COMPUTE WRK-4320E-RESTART  = (WRK-INP-PAGINA-9HTM - 2) * 10

           MOVE WRK-INP-BANCO-9HTM     TO WRK-4320E-BANCO
           MOVE WRK-INP-CODAGE-9HTM    TO WRK-4320E-DEPDC
           MOVE WRK-INP-CONTA-9HTM     TO WRK-4320E-CONTA

           MOVE 'DCOM4320'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4320E-ENTRADA
                                             WRK-4320S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4320S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4320S-CCONVE-GRAL-DESC(11)  NOT NUMERIC) OR
                      (WRK-4320S-CCONVE-GRAL-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9HTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9HTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9HTM

                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9HTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10
                   COMPUTE WRK-OUT-PAGINA-9HTM = WRK-INP-PAGINA-9HTM - 1

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4320S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9HTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2250-TELA-POSTERIOR             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-9HTM     EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MENSA-9HTM
               GO TO 2250-99-FIM
           END-IF.


           MOVE 'N'                    TO WRK-4320E-FLAG
           MOVE 'N'                    TO WRK-4320E-START-COUNT
           MOVE WRK-COD-USER           TO WRK-4320E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4320E-CTERM
           MOVE WRK-INP-CDPROD-9HTM    TO WRK-4320E-CPROD
           MOVE WRK-INP-CDSUBP-9HTM    TO WRK-4320E-CSPROD-DESC-COML.
           COMPUTE WRK-4320E-RESTART  = (WRK-INP-PAGINA-9HTM) * 10


           MOVE WRK-INP-BANCO-9HTM     TO WRK-4320E-BANCO
           MOVE WRK-INP-CODAGE-9HTM    TO WRK-4320E-DEPDC
           MOVE WRK-INP-CONTA-9HTM     TO WRK-4320E-CONTA

           MOVE 'DCOM4320'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4320E-ENTRADA
                                             WRK-4320S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4320S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4320S-CCONVE-GRAL-DESC(11)  NOT NUMERIC) OR
                      (WRK-4320S-CCONVE-GRAL-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9HTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9HTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9HTM
                       MOVE SG-MENSAGEM(01)

                                       TO WRK-OUT-MENSA-9HTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10
                   COMPUTE WRK-OUT-PAGINA-9HTM = WRK-INP-PAGINA-9HTM + 1

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4320S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9HTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS.

           PERFORM 2271-CONSISTIR-FISICA.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 2270-99-FIM
           END-IF.

           PERFORM 2272-TRANSFERIR-CONTROLE-0103.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*


           MOVE ZEROS                  TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-9HTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'          TO WRK-INCONSIS
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-9HTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)
                                     TO WRK-OUT-MENSA-9HTM
               ELSE
                   IF  WRK-INP-SELEC-9HTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-9HTM(WRK-IND)
                       ADD 1         TO WCT-SELEC
                       MOVE WRK-IND  TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM

           END-IF.



           IF  WCT-SELEC               EQUAL 0  OR  GREATER 1
               MOVE '*'                TO WRK-INCONSIS
               IF  WCT-SELEC           EQUAL 0
                   MOVE SG-MENSAGEM(53)
                                       TO WRK-OUT-MENSA-9HTM
               ELSE
                 IF  WCT-SELEC         GREATER 1
                     MOVE SG-MENSAGEM(39)
                                       TO WRK-OUT-MENSA-9HTM
                 END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-0103   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-103
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE LENGTH OF COMU-AREA-103
                                       TO COMU-LL-103
           MOVE WRK-INP-CHAMADO-9HTM   TO WRK-TELA
           MOVE WRK-INP-SENHAS-9HTM    TO COMU-SENHAS-103
           MOVE WRK-INP-COMANDO-9HTM   TO COMU-COMANDO-103
           MOVE WRK-INP-TIPOOPC-9HTM   TO COMU-OPCAO-103
           MOVE WRK-INP-TMSTAMP-9HTM   TO COMU-TIMESTAMP-103
           MOVE WRK-INP-PFK-9HTM       TO COMU-PFK-103
           MOVE WRK-INP-CODIGO-9HTM-N(WIND-SEL)
                                       TO COMU-COD-OPER-103
           MOVE 'C2'                   TO COMU-TPOPER-103
           MOVE 'DCOM3908'             TO COMU-TRANSACAO-103
           MOVE COMU-AREA-103          TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-9HTM
                                             WRK-660-DCOM9HTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9HTM        TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3908'         TO ERR-PGM

           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3908'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

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

           PERFORM 3000-FINALIZAR.


      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
