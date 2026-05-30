      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM3902.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3902                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: ADRIANO RICARDO BELMONT - ALTRAN.            *
      *     DATA........: 08/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA PARA SELECAO - SELECAO DE CLIENTE POR  *
      *                   CNPJ/CPF.                                    *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9BTM....: LISTA PARA SELECAO - SELECAO DE CLIENTE POR  *
      *                   CNPJ/CPF.                                    *

      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *     I#DCOMX4 - AREA PARA ACESSAR O MODULO DCOM6470 PARA        *
      *                OBTER A LISTA                                   *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM6470 - SELECAO DE CORRENTISTAS NO CADU                 *
      *     DCOM7995 - TRATAMENTO DE DB TEMPORARIO                     *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0102                                     *
BRQ001*                   DCOM0701                                     *
      *================================================================*
BRQ001*                   BRQ DIGITAL SOLUTIONS - ALTERACAO            *
BRQ001*----------------------------------------------------------------*

BRQ001*     PROGRAMADOR.: LUCIANDRA SILVEIRA                           *
BRQ001*     ANALISTA DS.: RICARDO JAMMAL - GRUPO 39                    *
BRQ001*     DATA........: 05/11/2018                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*     OBJETIVO....: ADEQUAR O PROGRAMA PARA RECEBER CONTROLE  DA *
BRQ001*                   TRANSACAO DCOM0701 (SIMULACAO DA OPERACAO) E *
BRQ001*                   CASO TENHA APENAS UM CLIENTE SELECIONA-LO RE *
BRQ001*                   TORNADO AO CHAMADOR.                         *
BRQ001*================================================================*
BRQ002*                   BRQ DIGITAL SOLUTIONS - ALTERACAO            *
BRQ002*----------------------------------------------------------------*
BRQ002*     ANALISTA BRQ: FERNANDO LUIZ DE SANTI                       *
BRQ002*     ANALISTA DS.: RICARDO JAMMAL - GRUPO 39                    *
BRQ002*     DATA........: 21/02/2019                                   *
BRQ002*----------------------------------------------------------------*
BRQ002*     OBJETIVO....: OBTER NOME DA AGENCIA DA CONTA SELECIONADA   *
BRQ002*       PARA PASSAR O CONTROLE.                                  *
BRQ002*----------------------------------------------------------------*
BRQ002*     MODULOS.....:                                              *
BRQ002*     MESU9015 - OBTER DADOS DE UMA AGENCIA.                     *
BRQ002*================================================================*


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

           '*** DCOM3902 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

       77  WRK-IND                     PIC  9(02) COMP-3   VALUE ZEROS.
       77  WIND-SEL                    PIC S9(09) COMP     VALUE ZEROS.
       77  WCT-SELEC                   PIC S9(09) COMP     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                     PIC S9(08) COMP     VALUE +225.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*


       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.
BRQ001     05  WRK-PASSAR-CTRL         PIC  X(001)         VALUE SPACES.
           05  WRK-MENSAGEM-ERRO.
               10  FILLER              PIC  X(15)          VALUE
                   'ERRO NO MODULO '.
               10  WRK-MODULO-ERRO     PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(13)          VALUE
                   ' - RETORNO = '.
               10  WRK-RETORNO-ERRO    PIC  X(04)          VALUE SPACES.
               10  FILLER              PIC  X(09)          VALUE
                   ' - MSG = '.
               10  WRK-MSG-ERRO        PIC  X(25)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM7995 **'.
      *----------------------------------------------------------------*


       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(0002)        VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(0009)        VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(0008)        VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(0026)        VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(0008)        VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(0008)        VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(0003)        VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(0002)        VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(0079)        VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(0026)        VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(0008)        VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(0008)        VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(0003)        VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE COMUNICACAO DCOM ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(04) COMP     VALUE +166.
           05  COMU-ZZ                 PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK            PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS             PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO            PIC  X(68)          VALUE SPACES.
           05  COMU-DADOS.
BRQ001         10  COMU-OPCAO          PIC  9(01)          VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(26)          VALUE SPACES.
               10  COMU-CNPJPRI.
4S2511*            15  COMU-CNPJPRI-N  PIC  9(009)         VALUE ZEROS.
4S2511             15  COMU-CNPJPRI-N  PIC  X(09)          VALUE SPACES.
               10  COMU-CNPJFIL.
4S2511*            15  COMU-CNPJFIL-N  PIC  9(004)         VALUE ZEROS.
4S2511             15  COMU-CNPJFIL-N  PIC  X(04)          VALUE SPACES.
               10  COMU-CNPJCTR.
                    15  COMU-CNPJCTR-N  PIC  9(002)         VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM0103 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-0103.
           05  COMU-LL-0103            PIC S9(004) COMP    VALUE +336.
           05  COMU-ZZ-0103            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-0103 PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-0103        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-0103       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-0103.
               10  COMU-OPCAO-0103     PIC  9(001)         VALUE ZEROS.
               10  COMU-TIMESTAMP-0103 PIC  X(026)         VALUE SPACES.
               10  COMU-NOME-0103      PIC  X(040)         VALUE SPACES.
               10  COMU-CPFCNPJ-0103.
4S2511*            15  COMU-CPFCNPJ-0103-N    PIC  9(009)   VALUE ZEROS.
4S2511             15  COMU-CPFCNPJ-0103-N    PIC  X(09)   VALUE SPACES.

               10  COMU-FILIAL-0103.
4S2511*              15  COMU-FILIAL-0103-      PIC 9(004) VALUE ZEROS.
4S2511               15  COMU-FILIAL-0103-N     PIC  X(04) VALUE SPACES.
               10  COMU-CONTROL-0103.
                    15  COMU-CONTROL-0103-N
                                      PIC  9(002)         VALUE ZEROS.
               10  COMU-BANCO-0103.
                   15  COMU-BANCO-0103-N
                                       PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA-0103.
                   15  COMU-AGENCIA-0103-N
                                       PIC  9(005)         VALUE ZEROS.
               10  COMU-DESCAGE-0103   PIC  X(030)         VALUE SPACES.
               10  COMU-CONTA-0103.
                   15  COMU-CONTA-0103-N
                                       PIC  9(013)         VALUE ZEROS.
               10  COMU-MENSAGE-0103   PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.

      *----------------------------------------------------------------*

       01  WRK-POOL5005.
      *    05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
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
           05  WRK-AGENCIA-AUX         REDEFINES  WRK-COD-DEPTO.
               10  FILLER              PIC  X(02).
               10  WRK-AGENCIA-AUX2    PIC  9(04).
           05  WRK-AGENCIA-5.
               10  WRK-AGENCIA-NUM     PIC  9(005)  VALUE ZEROS.


           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

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
           '*** AREA DA TELA DCOM9BTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOM9BTM.
           05  WRK-660-LL-AREA-9BTM    PIC  9(004) COMP    VALUE 0146.
           05  WRK-660-LL-MENSA-9BTM   PIC  9(004) COMP    VALUE 0955.
           05  WRK-660-SENHAS-9BTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-9BTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-9BTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TMSTAMP-9BTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-9BTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-ULTPAG-9BTM     PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-PAGINA-9BTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-CNPJPRI-9BTM    PIC  9(004) COMP    VALUE 0011.
           05  WRK-660-CNPJFIL-9BTM    PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-CNPJCTR-9BTM    PIC  9(004) COMP    VALUE 0002.
           05  WRK-600-LISTA           OCCURS  10.
               10 WRK-660-ESCOLH-9BTM  PIC  9(004) COMP    VALUE 2003.
               10 WRK-660-NOME-9BTM    PIC  9(004) COMP    VALUE 0025.

               10 WRK-660-AGENCIA-9BTM PIC  9(004) COMP    VALUE 0005.
               10 WRK-660-CONTA-9BTM   PIC  9(004) COMP    VALUE 0013.
               10 WRK-660-SITCAD-9BTM  PIC  9(004) COMP    VALUE 0015.
               10 WRK-660-DTVENC-9BTM  PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-9BTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9BTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-9BTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-9BTM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-9BTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-9BTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-9BTM.
               10  WRK-INP-TIPOOPC-9BTM
                                       PIC  9(01)          VALUE ZEROS.
               10  WRK-INP-TMSTAMP-9BTM
                                       PIC  X(26)          VALUE SPACES.

               10  WRK-INP-CHAMADO-9BTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-ULTPAG-9BTM PIC  X(01)          VALUE SPACE.
               10  WRK-INP-PAGINA-9BTM-X.
                   15  WRK-INP-PAGINA-9BTM
                                       PIC  9(04)          VALUE ZEROS.
               10  WRK-INP-CNPJPRI-9BTM.
                   15  WRK-INP-CNPJPRI-9BTM-N
                                       PIC  X(11)          VALUE SPACES.
               10  WRK-INP-CNPJFIL-9BTM.
4S2511*            15  WRK-INP-CNPJFIL-9BTM-N
4S2511*                                PIC  9(04)          VALUE ZEROS.
4S2511            15  WRK-INP-CNPJFIL-9BTM-N
4S2511                                 PIC  X(04)         VALUE SPACES.
               10  WRK-INP-CNPJCTR-9BTM.
                   15  WRK-INP-CNPJCTR-9BTM-N
                                       PIC  9(02)          VALUE ZEROS.
               10  WRK-INP-LINHAS-9BTM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-9BTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-INP-NOME-9BTM
                                       PIC  X(25)          VALUE SPACES.
                   15  WRK-INP-AGENCIA-9BTM.

                       20  WRK-INP-AGENCIA-9BTM-N
                                       PIC  9(05)          VALUE ZEROS.
                   15  WRK-INP-CONTA-9BTM.
                       20  WRK-INP-CONTA-9BTM-N
                                       PIC  9(13)          VALUE ZEROS.
                   15  WRK-INP-SITCAD-9BTM
                                       PIC  X(15)          VALUE SPACES.
                   15  WRK-INP-DTVENC-9BTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-INP-MENSA-9BTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOM9BTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-9BTM.
           05  WRK-OUT-LL-9BTM         PIC S9(04) COMP     VALUE +0955.
           05  WRK-OUT-ZZ-9BTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-9BTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-9BTM    PIC  X(68)          VALUE SPACES.

           05  WRK-OUT-DADOS-9BTM.
               10  WRK-OUT-TIPOOPC-9BTM
BRQ001                                 PIC  9(01)          VALUE ZEROS.
               10  WRK-OUT-TMSTAMP-9BTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-9BTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-ULTPAG-9BTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-9BTM-X.
                   15  WRK-OUT-PAGINA-9BTM
                                       PIC  9(04)          VALUE ZEROS.
               10  WRK-OUT-CNPJPRI-9BTM.
4S2511*            15  WRK-OUT-CNPJPRI-9BTM-N
4S2511*                                PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
4S2511            15  WRK-OUT-CNPJPRI-9BTM-N
4S2511                                 PIC  X(11)          VALUE SPACES.
               10  WRK-OUT-CNPJFIL-9BTM.
4S2511*            15  WRK-OUT-CNPJFIL-9BTM-N
4S2511*                                 PIC  9(04)          VALUE ZEROS.
4S2511            15  WRK-OUT-CNPJFIL-9BTM-N
4S2511                                 PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-CNPJCTR-9BTM.
                   15  WRK-OUT-CNPJCTR-9BTM-N
                                       PIC  9(02)          VALUE ZEROS.
               10  WRK-OUT-LINHAS-9BTM OCCURS 10 TIMES.

                   15  WRK-OUT-SELEC-ATTR-9BTM
                                       PIC S9(004) COMP    VALUE ZEROS.
                   15  WRK-OUT-SELEC-9BTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-OUT-NOME-9BTM
                                       PIC  X(25)          VALUE SPACES.
                   15  WRK-OUT-AGENCIA-9BTM.
                       20  WRK-OUT-AGENCIA-9BTM-N
                                       PIC  9(05)          VALUE ZEROS.
                   15  WRK-OUT-CONTA-9BTM.
                       20  WRK-OUT-CONTA-9BTM-N
                                       PIC  ZZZZZZZZZZZZ9  VALUE ZEROS.
                   15  WRK-OUT-SITCAD-9BTM
                                       PIC  X(15)          VALUE SPACES.
                   15  WRK-OUT-DTVENC-9BTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-OUT-MENSA-9BTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM6470 ***'.

      *----------------------------------------------------------------*

       01  ROTENT-6470-ENVIO.
           03 ROTENT-6470-COD-ENV        PIC  X(04).
      *      INFORMAR ZEROS
           03 ROTENT-6470-MSG-ENV        PIC  X(79).
      *      INFORMAR BRANCOS
           03 ROTENT-6470-REST-ENV       PIC  9(05).
      *      INFORMAR NUMERO DE REGISTROS PARA RESTART
           03 ROTENT-6470-FLAG-ENV       PIC  X(01).
      *      INFORMAR N
           03 ROTENT-6470-START-ENV      PIC  X(01).
      *      INFORMAR N NA PRIMEIRA CHAMADA E S A PARTIR DA SEGUNDA
           03 ROTENT-6470-CFUNC-ENV      PIC  9(09).
      *      INFORMAR CODIGO DO FUNCIONARIO
           03 ROTENT-6470-CTERM-ENV      PIC  X(08).
      *      INFORMAR TERMINAL
           03 ROTENT-6470-CJUNC-ENV      PIC  9(05).
           03 ROTENT-6470-CTAC-ENV.
              05 ROTENT-6470-CCTAC-ENV   PIC  9(07).
              05 ROTENT-6470-DCTAC-ENV   PIC  9(01).

4S2511*    03 ROTENT-6470-NUM-CNPJ-ENV   PIC  9(09).
4S2511     03 ROTENT-6470-NUM-CNPJ-ENV   PIC  X(09).
4S2511*    03 ROTENT-6470-FIL-CNPJ-ENV   PIC  9(04).
4S2511     03 ROTENT-6470-FIL-CNPJ-ENV   PIC  X(04).
           03 ROTENT-6470-CTR-CNPJ-ENV   PIC  9(02).
           03 ROTENT-6470-CCLUB-ENV      PIC  9(10).

      *********************************************************
      *AREA DE RETORNO                                        *
      *********************************************************

       01 ROTSAI-6470-RETORNO.
          03 ROTSAI-6470-COD-RET       PIC  X(04).
      *
      *            00 - REGISTRO ENCONTRADO
      *            02 - RESTART
      *            03 - NENHUM REGISTRO ENCONTRADO NO CADU
      *            99 - PROBLEMAS DB2
      *           100  DADOS INCONSISTENTES
      *
          03 ROTSAI-6470-MENS-RET      PIC  X(79).
          03 ROTSAI-6470-REST-RET      PIC  9(05).
          03 ROTSAI-6470-FLAG-RET      PIC  X(01).

          03 ROTSAI-6470-QTDE-RET      PIC  9(03).
          03 ROTSAI-6470-COUNT-RET     PIC  9(09).
          03 ROTSAI-TABELA-SAIDA     OCCURS    20  TIMES.
             05 ROTSAI-6470-NM-RZSOC-RET PIC  X(70).
             05 ROTSAI-6470-CCLUB-RET   PIC  9(10).
             05 ROTSAI-6470-CNPJ-CADU.
4S2511*         07 ROTSAI-CCNPJ-CPF      PIC  9(09).
4S2511          07 ROTSAI-CCNPJ-CPF      PIC  X(09).
4S2511*         07 ROTSAI-CFLIAL-CNPJ    PIC  9(04).
4S2511          07 ROTSAI-CFLIAL-CNPJ    PIC  X(04).
                07 ROTSAI-CCTRL-CNPJ-CPF PIC  9(02).
             05 ROTSAI-6470-TP-PESSOA   PIC  X(01).
             05 ROTSAI-6470-TIT-CLI-RET PIC  9(02).
             05 ROTSAI-6470-DATA-VENCTO PIC  X(10).
             05 ROTSAI-6470-SIT-OPE-RET PIC  X(20).
             05 ROTSAI-6470-CJUNC-RET   PIC  9(05).
             05 ROTSAI-6470-CTAC-RET.
                07 ROTSAI-6470-CCTAC-RET PIC 9(07).
                07 ROTSAI-6470-DCTAC-RET PIC 9(01).
             05 ROTSAI-6470-NOME-AGE    PIC  X(30).

BRQ002*---------------------------------------------------------------*
BRQ002 01  FILLER                      PIC  X(050)         VALUE

BRQ002     '*** AREA DE ACESSO AO MODULO MESU9015 ***'.
BRQ002*---------------------------------------------------------------*
BRQ002
BRQ002 COPY 'I#MESUM6'.
BRQ002
      *----------------------------------------------------------------*
      *    '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3902 - FIM DA AREA DE WORKING ***'.

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

           MOVE SPACES                 TO WRK-OUT-DADOS-9BTM
                                          WRK-OUT-SENHAS-9BTM
                                          WRK-OUT-COMANDO-9BTM

           INITIALIZE                     WRK-AUXILIARES
                                          ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO.

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

           MOVE 'DCOM9BTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3902'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9BTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9BTM'
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

           MOVE WRK-TRANSACAO          TO WRK-INP-CHAMADO-9BTM
                                          WRK-OUT-CHAMADO-9BTM.

           MOVE COMU-TIMESTAMP         TO  WRK-ENT-TIMESTAMP
           MOVE 'CS'                   TO  WRK-ENT-OPCAO.
           MOVE 'DCOM3902'             TO  WRK-ENT-TRANS.
           MOVE WRK-COD-USER-R         TO  WRK-ENT-COD-FUNC.
           MOVE LNK-IO-LTERM           TO  WRK-ENT-COD-TERMINAL.
           MOVE 1                      TO  WRK-ENT-PAGINA.

           PERFORM 2120-ACESSAR-DCOM7995.


           IF  WRK-SAI-COD-RETORNO  EQUAL  02
               MOVE WRK-SAI-DADOS   TO WRK-INP-DADOS-9BTM
               PERFORM 2205-DEVOLVER-TELA-FIXO
               PERFORM 2210-DEVOLVER-TELA-VARIA

BRQ001         IF  WRK-TRANSACAO        EQUAL 'DCOM0103' AND
BRQ001            (WRK-OUT-NOME-9BTM(1) NOT EQUAL SPACES AND
BRQ001             WRK-OUT-NOME-9BTM(2)     EQUAL SPACES)
BRQ001             PERFORM 2220-RETORNAR-CHAMADOR
BRQ001         ELSE
               MOVE SG-MENSAGEM(61) TO WRK-OUT-MENSA-9BTM
BRQ001         END-IF
               GO TO 2100-99-FIM
           END-IF.

           MOVE COMU-OPCAO             TO WRK-OUT-TIPOOPC-9BTM
BRQ001                                    WRK-INP-TIPOOPC-9BTM.
           MOVE COMU-TIMESTAMP         TO WRK-OUT-TMSTAMP-9BTM
BRQ001                                    WRK-INP-TMSTAMP-9BTM.
           MOVE 1                      TO WRK-OUT-PAGINA-9BTM

BRQ001                                    WRK-INP-PAGINA-9BTM.
           MOVE SPACES                 TO ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO
           INITIALIZE                     ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO
           MOVE 'N'                    TO ROTENT-6470-FLAG-ENV
           MOVE 'N'                    TO ROTENT-6470-START-ENV
           MOVE ZEROS                  TO ROTENT-6470-REST-ENV
                                          ROTENT-6470-CJUNC-ENV
                                          ROTENT-6470-CCTAC-ENV
                                          ROTENT-6470-CCLUB-ENV
           MOVE WRK-COD-USER           TO ROTENT-6470-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6470-CTERM-ENV
           MOVE COMU-CNPJPRI-N         TO ROTENT-6470-NUM-CNPJ-ENV
                                          WRK-OUT-CNPJPRI-9BTM-N
           MOVE COMU-CNPJFIL-N         TO ROTENT-6470-FIL-CNPJ-ENV
                                          WRK-OUT-CNPJFIL-9BTM-N
           MOVE COMU-CNPJCTR-N         TO ROTENT-6470-CTR-CNPJ-ENV
                                          WRK-OUT-CNPJCTR-9BTM-N
           MOVE WRK-AGENCIA-AUX2       TO WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-NUM        TO ROTENT-6470-MSG-ENV.

           MOVE 'DCOM6470'             TO WRK-MODULO

           CALL WRK-MODULO             USING ROTENT-6470-ENVIO
                                             ROTSAI-6470-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA
                                             LNK-IO-PCB
                                             LNK-ALT-PCB.

           EVALUATE ROTSAI-6470-COD-RET
              WHEN '0000'
                   IF (ROTSAI-6470-CJUNC-RET(11)  NOT NUMERIC) OR
                      (ROTSAI-6470-CJUNC-RET(11)  EQUAL ZEROS)
                       MOVE '*'         TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(61)
                                        TO WRK-OUT-MENSA-9BTM
                   ELSE
                       MOVE SPACES      TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(61)
                                        TO WRK-OUT-MENSA-9BTM
                   END-IF

                   PERFORM 2110-MONTAR-TELA
                           VARYING WRK-IND FROM 1 BY 1
                             UNTIL WRK-IND GREATER 10

BRQ001             IF ROTSAI-6470-NM-RZSOC-RET(1) NOT EQUAL SPACES AND
BRQ001                ROTSAI-6470-NM-RZSOC-RET(2) EQUAL SPACES
BRQ001                IF WRK-INP-CHAMADO-9BTM     EQUAL 'DCOM0701'
BRQ001                   PERFORM 2140-RETORNAR-PRIMEIRA-OCORR
BRQ001                ELSE
BRQ001                   MOVE 1                   TO WIND-SEL
BRQ001                   MOVE 'S'                 TO WRK-PASSAR-CTRL
BRQ001                   PERFORM 2272-TRANSFERIR-CONTROLE-0103
BRQ001                END-IF
BRQ001             END-IF

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE WRK-TRANSACAO  TO WRK-TELA

                   MOVE SPACES         TO COMU-DADOS-0103
                   MOVE COMU-OPCAO     TO COMU-OPCAO-0103
                   MOVE COMU-TIMESTAMP TO COMU-TIMESTAMP-0103
                   MOVE COMU-CNPJPRI-N TO COMU-CPFCNPJ-0103-N
                   MOVE COMU-CNPJFIL-N TO COMU-FILIAL-0103-N
                   MOVE COMU-CNPJCTR-N TO COMU-CONTROL-0103-N
                   MOVE ROTSAI-6470-MENS-RET
                                       TO COMU-MENSAGE-0103
                   MOVE LENGTH         OF COMU-AREA-0103
                                       TO COMU-LL-0103
                   MOVE WRK-CHNG       TO WRK-FUNCAO
                   MOVE 'DCOM3902'     TO COMU-TRANSACAO-0103
                   MOVE COMU-AREA-0103 TO WRK-MENSAGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *

      *----------------------------------------------------------------*
       2110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (ROTSAI-6470-CJUNC-RET(WRK-IND)  NOT NUMERIC) OR
              (ROTSAI-6470-CJUNC-RET(WRK-IND)  EQUAL ZEROS)
               MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-9BTM(WRK-IND)
               MOVE SPACES           TO WRK-OUT-NOME-9BTM(WRK-IND)
                                        WRK-OUT-AGENCIA-9BTM(WRK-IND)
                                        WRK-OUT-CONTA-9BTM(WRK-IND)
                                        WRK-OUT-DTVENC-9BTM(WRK-IND)
                                        WRK-OUT-SITCAD-9BTM(WRK-IND)
           ELSE
               MOVE ROTSAI-6470-NM-RZSOC-RET(WRK-IND)
                                     TO WRK-OUT-NOME-9BTM(WRK-IND)
BRQ001                                  WRK-INP-NOME-9BTM(WRK-IND)
               MOVE ROTSAI-6470-CJUNC-RET(WRK-IND)
                                     TO WRK-OUT-AGENCIA-9BTM-N(WRK-IND)
BRQ001                                  WRK-INP-AGENCIA-9BTM-N(WRK-IND)
               MOVE ROTSAI-6470-CCTAC-RET(WRK-IND)
                                     TO WRK-OUT-CONTA-9BTM-N(WRK-IND)

BRQ001                                  WRK-INP-CONTA-9BTM-N(WRK-IND)
               MOVE ROTSAI-6470-DATA-VENCTO(WRK-IND)
                                     TO WRK-OUT-DTVENC-9BTM(WRK-IND)
BRQ001                                  WRK-INP-DTVENC-9BTM(WRK-IND)
BRQ001         INSPECT WRK-INP-DTVENC-9BTM(WRK-IND)
BRQ001                                  REPLACING ALL '.' BY '/'
               INSPECT WRK-OUT-DTVENC-9BTM(WRK-IND)
                                        REPLACING ALL '.' BY '/'
               MOVE ROTSAI-6470-SIT-OPE-RET(WRK-IND)
                                     TO WRK-OUT-SITCAD-9BTM(WRK-IND)
BRQ001                                  WRK-INP-SITCAD-9BTM(WRK-IND)

           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DA TABELA TEMPORARIA              *
      *----------------------------------------------------------------*

       2120-ACESSAR-DCOM7995           SECTION.
      *----------------------------------------------------------------*

BRQ001*--> A OPCAO DE SIMULACAO DA OPERACAO TRABALHA COM A DBTEMPORARIO
BRQ001*--> LOGO A LISTA DE CLIENTE POR CPNJ N�O PODE EXCLUIR OS DADOS
BRQ001*--> ARMAZENADOS NO DCOM0701.
BRQ001
BRQ001     IF  WRK-INP-CHAMADO-9BTM    EQUAL 'DCOM0701'
BRQ001         GO TO 2120-99-FIM
BRQ001     END-IF.

           MOVE 'DCOM7995'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA-DCOM7995
                                          WRK-SAIDA-DCOM7995
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF WRK-SAI-COD-RETORNO      EQUAL 01
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-SAI-MSG-RETORNO TO ERR-TEXTO

              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
              IF WRK-SAI-COD-RETORNO   EQUAL 99
                 PERFORM 9999-PROCESSAR-ROTINA-ERRO
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001*     PASSA CONTROLE OU RETORNAR AO CHAMADOR ENVIANDO UNICA      *
BRQ001*     OCORRENCIA DA LISTA                                        *
BRQ001*----------------------------------------------------------------*
BRQ001 2140-RETORNAR-PRIMEIRA-OCORR    SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     INITIALIZE COMU-AREA-0103.
BRQ001
BRQ001     MOVE LENGTH                 OF COMU-AREA-0103

BRQ001                                 TO COMU-LL-0103.
BRQ001     MOVE COMU-OPCAO             TO COMU-OPCAO-0103.
BRQ001     MOVE COMU-TIMESTAMP         TO COMU-TIMESTAMP-0103.
BRQ001     MOVE WRK-OUT-NOME-9BTM(1)   TO COMU-NOME-0103.
BRQ001     MOVE COMU-CNPJPRI-N         TO COMU-CPFCNPJ-0103-N.
BRQ001     MOVE COMU-CNPJFIL-N         TO COMU-FILIAL-0103-N.
BRQ001     MOVE COMU-CNPJCTR-N         TO COMU-CONTROL-0103-N.
BRQ001     MOVE 237                    TO COMU-BANCO-0103-N.
BRQ001     MOVE WRK-OUT-AGENCIA-9BTM-N
BRQ001                             (1) TO COMU-AGENCIA-0103-N.
BRQ001     MOVE WRK-OUT-CONTA-9BTM-N
BRQ001                             (1) TO COMU-CONTA-0103-N.
BRQ001
BRQ001     MOVE WRK-CHNG               TO WRK-FUNCAO.
BRQ001     MOVE 'DCOM0701'             TO WRK-TELA.
BRQ001     MOVE 'DCOM3902'             TO COMU-TRANSACAO-0103.
BRQ001     MOVE COMU-AREA-0103         TO WRK-MENSAGEM.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 2140-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     PROCESSAR DCOM9BTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9BTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-9BTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

4S2511*   XINSPECT WRK-INP-CNPJPRI-9BTM(1:3)
4S2511*                                 REPLACING LEADING ' ' BY ZEROS.
4S2511*   XINSPECT WRK-INP-CNPJPRI-9BTM(5:3)
4S2511*                                 REPLACING LEADING ' ' BY ZEROS.
4S2511*   XINSPECT WRK-INP-CNPJPRI-9BTM(9:3)
4S2511*                                 REPLACING LEADING ' ' BY ZEROS.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10
                OR WRK-INP-CONTA-9BTM(WRK-IND) EQUAL SPACES
               INSPECT WRK-INP-AGENCIA-9BTM(WRK-IND)

                                      REPLACING LEADING ' ' BY ZEROS
               INSPECT WRK-INP-CONTA-9BTM(WRK-IND)
                                      REPLACING LEADING ' ' BY ZEROS
           END-PERFORM.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-9BTM      EQUAL 'H' OR
                   (WRK-INP-PFK-9BTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9BTM  EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-9BTM TO  WRK-OUT-MENSA-9BTM

               WHEN WRK-INP-PFK-9BTM     EQUAL '3' OR
                   (WRK-INP-PFK-9BTM     EQUAL '.' AND
                    WRK-INP-COMANDO-9BTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-9BTM     EQUAL '7' OR
                   (WRK-INP-PFK-9BTM     EQUAL '.' AND
                    WRK-INP-COMANDO-9BTM EQUAL 'PFK07')
                    PERFORM 2240-TELA-ANTERIOR


               WHEN WRK-INP-PFK-9BTM     EQUAL '8' OR
                   (WRK-INP-PFK-9BTM     EQUAL '.' AND
                    WRK-INP-COMANDO-9BTM EQUAL 'PFK08')
                    PERFORM 2250-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-9BTM          EQUAL '.' AND
                     WRK-INP-COMANDO-9BTM      EQUAL SPACES)
                     PERFORM 2270-TRATAR-ENTER
                     PERFORM 2210-DEVOLVER-TELA-VARIA

               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)
                                       TO  WRK-OUT-MENSA-9BTM
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

           MOVE WRK-INP-SENHAS-9BTM    TO WRK-OUT-SENHAS-9BTM
           MOVE WRK-INP-TIPOOPC-9BTM   TO WRK-OUT-TIPOOPC-9BTM
           MOVE WRK-INP-TMSTAMP-9BTM   TO WRK-OUT-TMSTAMP-9BTM
           MOVE WRK-INP-ULTPAG-9BTM    TO WRK-OUT-ULTPAG-9BTM
           MOVE WRK-INP-PAGINA-9BTM-X  TO WRK-OUT-PAGINA-9BTM-X
           MOVE WRK-INP-CHAMADO-9BTM   TO WRK-OUT-CHAMADO-9BTM
           MOVE WRK-INP-MENSA-9BTM     TO WRK-OUT-MENSA-9BTM

           MOVE WRK-INP-CNPJPRI-9BTM   TO WRK-OUT-CNPJPRI-9BTM

           MOVE WRK-INP-CNPJFIL-9BTM   TO WRK-OUT-CNPJFIL-9BTM
           MOVE WRK-INP-CNPJCTR-9BTM   TO WRK-OUT-CNPJCTR-9BTM.

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

               IF (WRK-INP-AGENCIA-9BTM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-AGENCIA-9BTM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-9BTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-9BTM(WRK-IND)
                                     TO WRK-OUT-SELEC-9BTM(WRK-IND)
                   MOVE WRK-INP-NOME-9BTM(WRK-IND)
                                     TO WRK-OUT-NOME-9BTM(WRK-IND)
                   MOVE WRK-INP-AGENCIA-9BTM-N(WRK-IND)

                                     TO WRK-OUT-AGENCIA-9BTM-N(WRK-IND)
                   MOVE WRK-INP-CONTA-9BTM-N(WRK-IND)
                                     TO WRK-OUT-CONTA-9BTM-N(WRK-IND)
                   MOVE WRK-INP-SITCAD-9BTM(WRK-IND)
                                     TO WRK-OUT-SITCAD-9BTM(WRK-IND)
                   MOVE WRK-INP-DTVENC-9BTM(WRK-IND)
                                     TO WRK-OUT-DTVENC-9BTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR                              *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE 'EP'                   TO WRK-ENT-OPCAO.

           MOVE WRK-INP-TMSTAMP-9BTM   TO WRK-ENT-TIMESTAMP.
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC.
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL.
           MOVE 'DCOM3902'             TO WRK-ENT-TRANS.
           MOVE 1                      TO WRK-ENT-PAGINA.

           PERFORM 2120-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA
           MOVE SPACES                 TO COMU-DADOS
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE LENGTH OF COMU-AREA    TO COMU-LL

           IF  WRK-INP-CHAMADO-9BTM    EQUAL 'DCOM0103'
BRQ001         MOVE 'DCOM0101'         TO WRK-TELA
           ELSE
               IF WRK-INP-CHAMADO-9BTM EQUAL 'DCOM1002'
                  MOVE 'DCOM1001'      TO WRK-TELA
               ELSE
                  MOVE WRK-INP-CHAMADO-9BTM

                                       TO WRK-TELA
               END-IF
           END-IF.

           MOVE 'DCOM3902'             TO COMU-TRANSACAO
           MOVE '3'                    TO COMU-PFK
           MOVE WRK-INP-TIPOOPC-9BTM   TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-9BTM   TO COMU-TIMESTAMP
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2240-TELA-ANTERIOR              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-9BTM  EQUAL 1

               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MENSA-9BTM
               GO TO 2240-99-FIM
           END-IF.

           MOVE SPACES                 TO ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO

           INITIALIZE                     ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO
           MOVE 'N'                    TO ROTENT-6470-FLAG-ENV
           MOVE 'S'                    TO ROTENT-6470-START-ENV
           MOVE WRK-COD-USER           TO ROTENT-6470-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6470-CTERM-ENV
           MOVE WRK-AGENCIA-AUX2       TO WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-NUM        TO ROTENT-6470-MSG-ENV.

           MOVE WRK-INP-CNPJPRI-9BTM-N(1:3)
                                    TO ROTENT-6470-NUM-CNPJ-ENV(1:3)
           MOVE WRK-INP-CNPJPRI-9BTM-N(5:3)
                                    TO ROTENT-6470-NUM-CNPJ-ENV(4:3)

           MOVE WRK-INP-CNPJPRI-9BTM-N(9:3)
                                    TO ROTENT-6470-NUM-CNPJ-ENV(7:3)

           MOVE WRK-INP-CNPJFIL-9BTM-N TO ROTENT-6470-FIL-CNPJ-ENV
           MOVE WRK-INP-CNPJCTR-9BTM-N TO ROTENT-6470-CTR-CNPJ-ENV
           MOVE ZEROS                  TO ROTENT-6470-CJUNC-ENV
                                          ROTENT-6470-CCTAC-ENV
                                          ROTENT-6470-CCLUB-ENV
           COMPUTE ROTENT-6470-REST-ENV = (WRK-INP-PAGINA-9BTM - 2) * 10

           MOVE 'DCOM6470'             TO WRK-MODULO

           CALL WRK-MODULO             USING ROTENT-6470-ENVIO
                                             ROTSAI-6470-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA
                                             LNK-IO-PCB
                                             LNK-ALT-PCB.
           EVALUATE ROTSAI-6470-COD-RET
              WHEN '0000'
                   IF (ROTSAI-6470-CJUNC-RET(11)  NOT NUMERIC) OR

                      (ROTSAI-6470-CJUNC-RET(11)  EQUAL ZEROS)
                       MOVE '*'         TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(17)
                                        TO WRK-OUT-MENSA-9BTM
                   ELSE
                       MOVE SPACES      TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(01)
                                        TO WRK-OUT-MENSA-9BTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10
                   COMPUTE WRK-OUT-PAGINA-9BTM = WRK-INP-PAGINA-9BTM - 1
              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
              WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE ROTSAI-6470-MENS-RET
                                       TO WRK-OUT-MENSA-9BTM
           END-EVALUATE.


      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2250-TELA-POSTERIOR             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-9BTM     EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MENSA-9BTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE SPACES                 TO ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO.

           INITIALIZE                     ROTENT-6470-ENVIO
                                          ROTSAI-6470-RETORNO

           MOVE 'N'                    TO ROTENT-6470-FLAG-ENV
           MOVE 'S'                    TO ROTENT-6470-START-ENV
           MOVE WRK-COD-USER           TO ROTENT-6470-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6470-CTERM-ENV
           MOVE WRK-AGENCIA-AUX2       TO WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-NUM        TO ROTENT-6470-MSG-ENV.

           MOVE WRK-INP-CNPJPRI-9BTM-N(1:3)
                                     TO ROTENT-6470-NUM-CNPJ-ENV(1:3)
           MOVE WRK-INP-CNPJPRI-9BTM-N(5:3)
                                     TO ROTENT-6470-NUM-CNPJ-ENV(4:3)
           MOVE WRK-INP-CNPJPRI-9BTM-N(9:3)
                                     TO ROTENT-6470-NUM-CNPJ-ENV(7:3)

           MOVE WRK-INP-CNPJFIL-9BTM-N TO ROTENT-6470-FIL-CNPJ-ENV
           MOVE WRK-INP-CNPJCTR-9BTM-N TO ROTENT-6470-CTR-CNPJ-ENV
           MOVE ZEROS                  TO ROTENT-6470-CJUNC-ENV
                                          ROTENT-6470-CCTAC-ENV
                                          ROTENT-6470-CCLUB-ENV
           COMPUTE ROTENT-6470-REST-ENV = (WRK-INP-PAGINA-9BTM) * 10


           MOVE 'DCOM6470'             TO WRK-MODULO
           CALL WRK-MODULO             USING ROTENT-6470-ENVIO
                                             ROTSAI-6470-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA
                                             LNK-IO-PCB
                                             LNK-ALT-PCB.

           EVALUATE ROTSAI-6470-COD-RET
              WHEN '0000'
                   IF (ROTSAI-6470-CJUNC-RET(11)  NOT NUMERIC) OR
                      (ROTSAI-6470-CJUNC-RET(11)  EQUAL ZEROS)
                       MOVE '*'         TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(17)
                                        TO WRK-OUT-MENSA-9BTM
                   ELSE
                       MOVE SPACES      TO WRK-OUT-ULTPAG-9BTM
                       MOVE SG-MENSAGEM(01)
                                        TO WRK-OUT-MENSA-9BTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1

                                              UNTIL WRK-IND GREATER 10
                   COMPUTE WRK-OUT-PAGINA-9BTM = WRK-INP-PAGINA-9BTM + 1

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE ROTSAI-6470-MENS-RET
                                       TO WRK-OUT-MENSA-9BTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*


           MOVE  SPACES                TO WRK-INCONSIS
BRQ001     MOVE 'N'                    TO WRK-PASSAR-CTRL

           PERFORM 2271-CONSISTIR-FISICA

           IF  WRK-INCONSIS            EQUAL  '*'
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

               IF  WRK-INP-SELEC-9BTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'          TO WRK-INCONSIS
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-9BTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)
                                     TO WRK-OUT-MENSA-9BTM
               ELSE
                   IF  WRK-INP-SELEC-9BTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-9BTM(WRK-IND)
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
                                       TO WRK-OUT-MENSA-9BTM
               ELSE
                 IF  WCT-SELEC         GREATER 1
                     MOVE 'SELECIONAR APENAS UMA OPCAO'
                                       TO WRK-OUT-MENSA-9BTM
                 END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-0103   SECTION.
      *----------------------------------------------------------------*


BRQ001     IF  WRK-INP-CHAMADO-9BTM    EQUAL 'DCOM0701'
BRQ001         MOVE 'DCOM0701'         TO WRK-TELA
BRQ001     ELSE
               MOVE SPACES             TO WRK-INP-SELEC-9BTM(WIND-SEL)

               PERFORM 2273-GRAVAR-TAB-TEMP

               INITIALIZE COMU-AREA-0103

               IF  WRK-INP-CHAMADO-9BTM    EQUAL 'DCOM1001'
                   MOVE 'DCOM1002'         TO WRK-TELA
               ELSE
                   MOVE 'DCOM0103'         TO WRK-TELA
               END-IF
BRQ001     END-IF.

           MOVE WRK-INP-TIPOOPC-9BTM   TO COMU-OPCAO-0103
           MOVE WRK-INP-TMSTAMP-9BTM   TO COMU-TIMESTAMP-0103
           MOVE WRK-INP-NOME-9BTM(WIND-SEL)
                                       TO COMU-NOME-0103
           MOVE WRK-INP-CNPJPRI-9BTM-N(1:3)
                                       TO COMU-CPFCNPJ-0103-N(1:3)
           MOVE WRK-INP-CNPJPRI-9BTM-N(5:3)
                                       TO COMU-CPFCNPJ-0103-N(4:3)
           MOVE WRK-INP-CNPJPRI-9BTM-N(9:3)
                                       TO COMU-CPFCNPJ-0103-N(7:3)

BRQ001     IF  WRK-PASSAR-CTRL         EQUAL 'S'
BRQ001         MOVE COMU-CNPJPRI-N     TO COMU-CPFCNPJ-0103-N
BRQ001         MOVE COMU-CNPJFIL-N     TO WRK-INP-CNPJFIL-9BTM-N
BRQ001         MOVE COMU-CNPJCTR-N     TO WRK-INP-CNPJCTR-9BTM-N
BRQ001     END-IF.
BRQ001
           MOVE WRK-INP-CNPJFIL-9BTM-N TO COMU-FILIAL-0103-N
           MOVE WRK-INP-CNPJCTR-9BTM-N TO COMU-CONTROL-0103-N
           MOVE 237                    TO COMU-BANCO-0103-N
BRQ002
BRQ002     PERFORM 2274-OBTER-NOME-AGENCIA.

           INSPECT WRK-INP-AGENCIA-9BTM(WIND-SEL)
                                  REPLACING LEADING ' ' BY ZEROS
           INSPECT WRK-INP-CONTA-9BTM(WIND-SEL)

                                  REPLACING LEADING ' ' BY ZEROS

           MOVE WRK-INP-AGENCIA-9BTM-N(WIND-SEL)
                                       TO COMU-AGENCIA-0103-N
           MOVE WRK-INP-CONTA-9BTM-N(WIND-SEL)
                                       TO COMU-CONTA-0103-N

           MOVE LENGTH OF COMU-AREA-0103
                                       TO COMU-LL-0103
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM3902'             TO COMU-TRANSACAO-0103
           MOVE COMU-AREA-0103         TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA A GRAVACAO DA TABELA TEMPORARIA            *
      *----------------------------------------------------------------*
       2273-GRAVAR-TAB-TEMP            SECTION.

      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'I '                   TO WRK-ENT-OPCAO.
           MOVE 'DCOM3902'             TO WRK-ENT-TRANS.
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC.
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL.
           MOVE WRK-INP-TMSTAMP-9BTM   TO WRK-ENT-TIMESTAMP.
           MOVE 1                      TO WRK-ENT-PAGINA.
           MOVE WRK-INP-DADOS-9BTM     TO WRK-ENT-DADOS.

           PERFORM 2120-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       2273-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ002*----------------------------------------------------------------*
BRQ002* OBTER NOME DA AGENCIA DA OCORRENCIA SELECIONADA                *

BRQ002*----------------------------------------------------------------*
BRQ002 2274-OBTER-NOME-AGENCIA         SECTION.
BRQ002*----------------------------------------------------------------*
BRQ002
BRQ002     INSPECT WRK-INP-AGENCIA-9BTM(WIND-SEL)
BRQ002                                 REPLACING LEADING ' ' BY ZEROS.
BRQ002
BRQ002     INITIALIZE COMU-PARM
BRQ002                ERRO-AREA
BRQ002                WRK-SQLCA.
BRQ002
BRQ002     MOVE 237                    TO COMU-BANCO.
BRQ002     MOVE WRK-INP-AGENCIA-9BTM-N(WIND-SEL)
BRQ002                                 TO COMU-DEPDC.
BRQ002     MOVE 'DCOM3902'             TO COMU-NOME-PGM.
BRQ002     MOVE 'MESU9015'             TO WRK-MODULO
BRQ002
BRQ002     CALL WRK-MODULO             USING COMU-PARM
BRQ002                                       ERRO-AREA
BRQ002                                       WRK-SQLCA.
BRQ002

BRQ002     EVALUATE COMU-COD-RETORNO
BRQ002       WHEN 00
BRQ002         MOVE COMU-NOME-DEPDC    TO COMU-DESCAGE-0103
BRQ002
BRQ002       WHEN 88
BRQ002         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BRQ002         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ002
BRQ002       WHEN OTHER
BRQ002         MOVE SPACES             TO COMU-DESCAGE-0103
BRQ002     END-EVALUATE.
BRQ002
BRQ002*----------------------------------------------------------------*
BRQ002 2274-99-FIM.                    EXIT.
BRQ002*----------------------------------------------------------------*
BRQ002
      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-9BTM

                                             WRK-660-DCOM9BTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9BTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM3902'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3902'         TO ERR-MODULO
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
