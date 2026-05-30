      *-----------------------------------------------------------------
      * BOOK DA AREA DE COMUNICACAO (NAVEGACAO) ENTRE PROGRAMAS DO RUEC
      *-----------------------------------------------------------------


       01  CTERM                              PIC X(0008)  VALUE SPACES.


       01  NAVEG-RURAL                        PIC X(4000)  VALUE SPACES.
       01  NAVEGACAO-RURAL                    REDEFINES    NAVEG-RURAL.


     ***       600 BYTES REF. A CAMPOS UTILIZADOS NO RUEC7000

PRI499     02  AREA-COMUM                             PIC X(600).
           02  AREA-RUEC7000                  REDEFINES    AREA-COMUM.

               05  PROGRAMA-ANT                       PIC X(08).
               05  AGENCIA                            PIC 9(05)  COMP-3.
               05  CONTA                              PIC 9(07)  COMP-3.
               05  TIPO-PESSOA                        PIC X(01).
     ***       = 'J' QUANDO CTPO-PSSOA (OBJEV000) = '1'
     ***       = 'F' QUANDO CTPO-PSSOA (OBJEV000) DIFERENTE DE '1'
               05  IDENTIF                            PIC 9(15).
               05  CGC                        REDEFINES    IDENTIF.
                   07  NUMCGC                         PIC 9(09).
                   07  FILIAL                         PIC 9(04).
                   07  CTRL-CGC                       PIC 9(02).
               05  CPF                        REDEFINES    IDENTIF.
                   07  NUMCPF                         PIC 9(09).
                   07  CTRL-CPF                       PIC 9(02).
                   07  FILLER                         PIC 9(04).
               05  NOME                               PIC X(40).
               05  SITUACAO                           PIC X(11).
PRI499         05  MENS-SITUACAO                      PIC X(26).
               05  COD-IMPEDIMENTO                    PIC 9(03).
PW9931         05  COD-MODALIDADE                     PIC 9(05)  COMP-3.
PW9931         05  COD-VARIEDADE                      PIC 9(05)  COMP-3.
PW9931         05  COD-CESTA-SAFRA                    PIC 9(05)  COMP-3.
PW9931         05  COD-TIPO-AGROPEC                   PIC 9(05)  COMP-3.
PW9931         05  COD-TIPO-INTEGR                    PIC 9(05)  COMP-3.
PW9931         05  COD-TIPO-IRRIG                     PIC 9(05)  COMP-3.
PW9931         05  COD-TIPO-CULTIVO                   PIC 9(05)  COMP-3.
PW9931         05  COD-CICLO-PROD                     PIC 9(05)  COMP-3.
PW9931         05  COD-ZONEAMENTO                     PIC X(01).
PW9931         05  COD-CONSORCIO                      PIC X(01).
POL097         05  CCLUB-RUEC                         PIC 9(10).
PW9931         05  COD-GRAO-SEMTE                     PIC 9(01).
PW9931         05  COD-TIPO-CEDULA                    PIC 9(01).
STEFA1         05  QUND-ANMAL                         PIC 9(09)V99.
STEFA1         05  FILLER                             PIC X(01).
     ***       "S" ====> CONTINUA O PROCESSO
     ***       "N" ====> NAO CONTINUA E EMITE A MSG DE ALERTA
STEFA1*        X5  FILLER                             PIC X(12).
STEFAB         05  COD-TP-MANEJO                      PIC 9(02).
               05  COD-PRODUTO                        PIC 9(05)  COMP-3.
               05  PRODUTO                            PIC X(40).
               05  COD-FINALIDADE                     PIC 9(03)  COMP-3.
               05  FINALIDADE                         PIC X(40).
               05  COD-APLICACAO                      PIC 9(03)  COMP-3.
               05  COD-UNID-FINAN                     PIC 9(03)  COMP-3.
               05  COD-UNID-PROD-PROV                 PIC 9(03)  COMP-3.
               05  FLAG-AREA-QTDE                     PIC 9(01).
     ***       "1" ====> QTDE
     ***       "2" ====> AREA
     ***       "3" ====> NENHUM
               05  FLAG-PROD-PROVAVEL                 PIC 9(01).
     ***       "0" ====> NAO TEM
     ***       "1" ====> EM HA
     ***       "2" ====> TOTAL
               05  COD-ORIG-REC                       PIC 9(03)  COMP-3.
               05  ORIG-REC                           PIC X(40).
               05  COD-ATIVIDADE                      PIC 9(03)  COMP-3.
               05  ATIVIDADE                          PIC X(20).
               05  AREA-FINANC                      PIC 9(15)V99 COMP-3.
               05  QTDE-FINANC                      PIC 9(15)V99 COMP-3.
               05  VALOR-FINANC                     PIC 9(15)V99 COMP-3.
               05  VGARNT                           PIC 9(15)V99 COMP-3.
               05  PROD-PROVAVEL                    PIC 9(15)V99 COMP-3.
               05  COD-TIPO-PRODUTOR          PIC 9(02).
               05  QTDE-CONTR-ORIGEM          PIC 9(03)    COMP-3.
               05  SOMA-VALORES-CONTR               PIC 9(15)V99 COMP-3.
               05  CCDULA                     PIC 9(09)    COMP-3.
               05  CHAMADA-BLOQUEIO.
                   07  PROGRAMA-BLOQUEIO              PIC X(08).
                   07  CONSULTA-BLOQUEIO              PIC 9(03) COMP-3.
               05  RETORNO-BLOQUEIO.
                   07  CODIGO-BLOQUEIO                PIC 9(01).
     ***               0 = BLOQUEIO TOTAL (DEVE IMPEDIR A CONTINUIDADE)
     ***               1 = BLOQUEIO GERA INCONSISTENCIA
     ***               2 = NAO HA BLOQUEIO
     ***               3 = ERRO DB2 (EMITIR GOBACK)
                   07  QTDE-BLOQUEIO                  PIC 9(02).
                   07  MSG-RETORNO-BLOQ               PIC X(40).
                   07  RESP-APROV-INCONS              PIC 9(01).
     ***               0 = BLOQUEIO TOTAL (DEVE IMPEDIR A CONTINUIDADE)
     ***               1 = DCR
     ***               2 = AGRONOMO
     ***               3 = AGENCIA
               05  TIPO-EXPLOR-RURAL                  PIC X(20).
               05  FLAG-GRAVA-IMOV                    PIC X(01).
               05  FLAG-GRAVA-INCONS                  PIC X(01).
               05  CTPO-GARNT                         PIC 9(03)  COMP-3.
               05  CESPCE-TITLO-GARNT                 PIC 9(03)  COMP-3.
               05  CGARTD                             PIC 9(03)  COMP-3.
               05  DINCL                              PIC X(10).
               05  CTPO-EMBAL                         PIC 9(03)  COMP-3.
               05  CCATEG-MUTUA                       PIC 9(03)  COMP-3.
               05  CPROJ-RURAL                        PIC 9(03)  COMP-3.
               05  DINIC-SAFRA                        PIC 9(06).
     ***       MMSSAA (MES,SECULO,ANO)
               05  DFIM-SAFRA                         PIC 9(06).
     ***       MMSSAA (MES,SECULO,ANO)
               05  CINDCD-RESP-APROV                  PIC 9(01).
               05  CLASSIF-PRODT                      PIC 9(03).
               05  EXIBE-MENU-8600                    PIC X(01).
     ***       WNAV-FUNCAO (C --> COMPLEMENTACAO / A --> ALTERACAO
               05  WNAV-FUNCAO                        PIC X(01).
     ***       PARAMETROS PASSADOS PARA O MODULO DE CONTROLE DE PASSO
     ***       IND --> CONTADOR DE PARAMETROS PASSADOS
     ***       NUMORD --> NUMERO DE ORDEM
     ***       INDSEL --> INDICADOR DE SELECAO
               05  PAR-IND                            PIC 9(02).
               05  PAR-MCP                            OCCURS 24 TIMES.
                   06  NUM-ORD                        PIC 9(02).
                   06  IND-SEL                        PIC 9(01).
               05  CUF                                PIC 9(03) COMP-3.
               05  CSEGDR                             PIC 9(03) COMP-3.
               05  RECEITA-BRUTA                    PIC 9(15)V99 COMP-3.
LMC            05  COD-TIPO-BENEFIC                   PIC 9(02).
4SYS           05  FORMA-LIQ-CONTR                    PIC 9(02).
4SYS ***           FORMA DE LIQUIDACAO DAS PARCELAS DO CONTRATO
4SYS ***           = 01 - DEBITO EM CONTA
4SYS ***           = 02 - BOLETO
4SYS           05  INDC-OPTE-CHEQ                     PIC 9(01).
4SYS ***           CLIENTE PERMITE O USO DO LIMITE DE CHEQUE ESPECIAL
4SYS ***           =  1 - SIM
4SYS ***           =  2 - NAO
4SYS           05  INDC-OPTE-REENV                    PIC 9(01).
4SYS ***           CLIENTE PERMITE A COBRANCA EM DEBITOS PARCIAIS NA
4SYS ***           INADIMPLENCIA
4SYS ***           =  1 - SIM
4SYS ***           =  2 - NAO
WIPRO*** INFORMACOES DA ZARC - ZONEAMENTO AGRICOLA DE RISCO CLIMATICO
WIPRO*** OBTIDOS QUANDO ZONEAMENTO = S - NO PROGRAMA RUEC9310
WIPRO          05  RUEC00-ZARC.
WIPRO*** CICLO DO CULTIVAR
WIPRO              10  RUEC00-CCICLO-CTVDA            PIC 9(02).
WIPRO*** TIPO DE SOLO
WIPRO              10  RUEC00-CTPO-SOLO-AGCLT         PIC 9(02).
WIPRO*** TIPO DE MANEJO
WIPRO              10  RUEC00-CMANUT-SOLO             PIC 9(02).
PRI499         05  COD-CLIMA-PROD                     PIC 9(02).

     ***       3400 BYTES UTILIZADOS PARA DIVERSOS PROGRAMAS

           02  AREA-TOTAL.


POL097       03   AREA-REDEFINICAO                    PIC X(1550).

             03   DADOS-IMOVEIS              REDEFINES AREA-REDEFINICAO.

     ***       1225 BYTES REF. AOS DADOS DOS IMOVEIS

               05  IMOVEIS                    OCCURS  25    TIMES.
                   07  CMATR-IMOV                     PIC X(10).
                   07  IIMOV-RURAL                    PIC X(025).
                   07  CMUN-RURAL                     PIC 9(09) COMP-3.
                   07  CINDCD-AREA-PRINC              PIC 9(01).
     ***           = '0' QUANDO FOR O IMOVEL PRINCIPAL
     ***           = '1' PARA OS DEMAIS IMOVEIS
                   07  CSEQ-IMOV-RURAL                PIC 9(03) COMP-3.
                   07  MAREA-IMOV-RURAL             PIC 9(07)V99 COMP-3.


               05  IMOVEIS-A-HIPOTECAR        OCCURS  25    TIMES.
POL097             07  CSEQ-PATRM-IMOV-HIPOT          PIC 9(005) COMP-3.
POL097             07  ORIG-IMOV-HIPOT                PIC X(001).
                   07  INDIC-HIPOT-PROCESSADA         PIC X(001).
     ***                   S = HIPOTECADA JA INCLUIDA NO DB
     ***                   N = HIPOTECADA AINDA NAO INCLUIDA NO DB


POL097**           156 BYTES REF. "SAVE AREA" TELA RUECT753


               05  TELA-RUEC7530-GRUPO.
                   07  TELA-RUEC7530                  OCCURS  12  TIMES.
                      09  7530-CSEQ-IMOV-RURAL        PIC 9(03) COMP-3.
                      09  7530-CMUN-RURAL             PIC 9(09) COMP-3.
                      09  7530-MAREA-IMOV-RURAL     PIC 9(07)V99 COMP-3.
                      09  7530-DESPROTEGE-SEL         PIC X(001).


               05  TELA-RUEC7110-GRUPO.
                   07  TELA-RUEC7110                  OCCURS  12  TIMES.
POL097                09  7110-CSEQ-PATRM-IMOV        PIC 9(005) COMP-3.
POL097                09  7110-ORIG-IMOV              PIC X(001).
                      09  7110-INDIC-HIPOT-PROCES     PIC X(001).

POL097         05  FILLER                             PIC X(009).


     ***          120 BYTES REF. ASSISTENCIA TECNICA E AGRONOMO

             03   DADOS-ENCARGOS             REDEFINES AREA-REDEFINICAO.

               05  JUROS.
                   07  CTPO-EVNTO-RURAL-J             PIC 9(4).
                   07  VEVNTO-RURAL-J               PIC 9(13)V99 COMP-3.
               05  SAVE-JUROS.
                   07  SAVE-CTPO-EVNTO-RURAL-J        PIC 9(4).
                   07  SAVE-VEVNTO-RURAL-J          PIC 9(13)V99 COMP-3.
               05  CORRECAO.
                   07  CTPO-EVNTO-RURAL-CM            PIC 9(4).
                   07  VEVNTO-RURAL-CM              PIC 9(13)V99 COMP-3.
               05  ASSIST-TEC.
                   07  CTPO-EVNTO-RURAL-A             PIC 9(4).
                   07  VEVNTO-RURAL-A               PIC 9(13)V99 COMP-3.
               05  IOF.
                   07  CTPO-EVNTO-RURAL-I             PIC 9(4).
                   07  VEVNTO-RURAL-I               PIC 9(13)V99 COMP-3.
               05  AGRONOMO.
                   07  CCREA                          PIC 9(10).
                   07  CRGIAO-CREA                    PIC X(02).
                   07  IAGRON                         PIC X(60).
               05  TIMESTAMP-RUEC7020                 PIC X(26).
               05  DADOS-MUNICIPIO.
                   07  CMUN-EGF-ESTOC                 PIC 9(05).
                   07  IMUN-EGF-ESTOC                 PIC X(39).

POL097         05  FILLER                             PIC X(1348).


             03   DADOS-AVALISTA-EMITENTE    REDEFINES AREA-REDEFINICAO.

     ***           280 BYTES REF. A AVALISTAS

               05  OCORRENCIAS-AVALISTAS      OCCURS  4.
                   07  AGEN-AVAL                      PIC 9(04).
                   07  CONTA-AVAL                     PIC 9(04).
                   07  CGCCPF-AVAL                    PIC 9(09) COMP-3.
                   07  FILIAL-AVAL                    PIC 9(04).
                   07  CTRL-CPF-AVAL                  PIC 9(02).
                   07  NOME-AVAL                      PIC X(40).
                   07  SITUACAO-AVAL                  PIC X(11).

     ***           210 BYTES REF. A EMITENTES SOLIDARIOS

               05  OCORRENCIAS-EMITENTES      OCCURS  3.
                   07  AGEN-EMIT                      PIC 9(04).
                   07  CONTA-EMIT                     PIC 9(04).
                   07  CGCCPF-EMIT                    PIC 9(09) COMP-3.
                   07  FILIAL-EMIT                    PIC 9(04).
                   07  CTRL-CPF-EMIT                  PIC 9(02).
                   07  NOME-EMIT                      PIC X(40).
                   07  SITUACAO-EMIT                  PIC X(11).

POL097         05  FILLER                             PIC X(1060).

POL097       03   AREA-SALVA-TELA1                    PIC X(1050).
             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7530.

                   07  HOLD-OCORRENCIAS               OCCURS  12.
                      09  HOLD-7530-SELECIONA         PIC X(01).
                      09  HOLD-7530-DESPROTEGE-SEL    PIC X(01).
                      09  HOLD-7530-DENOMINACAO       PIC X(25).
                      09  HOLD-7530-MATRICULA         PIC X(10).
                      09  HOLD-7530-MUNICIPIO         PIC X(15).
                      09  HOLD-7530-UF                PIC X(02).
                      09  HOLD-7530-STATUS            PIC X(20).

                   07  HOLD-7530-TIPO-EXPLOR-RURAL    PIC X(20).
                   07  HOLD-7530-MSG                  PIC X(79).
                   07  HOLD-7530-CURSOR.
                      09  HOLD-7530-LINHA             PIC 9(04) COMP.
                      09  HOLD-7530-COLUNA            PIC 9(04) COMP.
                   07  HOLD-7530-CSEQ-IMOV-RURAL      PIC 9(03) COMP-3.
                   07  HOLD-7530-PAGINA               PIC 9(03) COMP-3.
POL097             07  FILLER                         PIC X(55).


             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  DADOS-GARANTIA.
                   07  7090-ITPO-GARNT                PIC X(040).
                   07  7090-IESPCE-TITLO-GARNT        PIC X(040).
                   07  7100-RROTRO-ACSSO              PIC X(150).

               05  DADOS-GARANTIDOR.
                   07  7090-CTPO-GARTD                PIC 9(001).
                   07  7090-IPSSOA                    PIC X(060).

               05  HOLD-TELA-RUEC7090.
                   07  HOLD-7090-TIMESTAMP            PIC X(26).
                   07  HOLD-7090-ITPO-GARNT           PIC X(40).
                   07  HOLD-7090-IESPCE-TITLO-GARNT   PIC X(40).
                   07  HOLD-7090-NUM-CONS-CONE        PIC 9(18).
                   07  HOLD-7090-CONVENIO-COD         PIC 9(03).
                   07  HOLD-7090-CONVENIO-NOME        PIC X(60).
                   07  HOLD-7090-CONVENIO-LIT         PIC X(18).
                   07  HOLD-7090-FIANCA-COD           PIC 9(03).
                   07  HOLD-7090-FIANCA-NOME          PIC X(60).
                   07  HOLD-7090-FIANCA-LIT           PIC X(18).
                   07  HOLD-7090-ITPGAR               PIC X(01).

                   07  HOLD-DATA                      PIC X(10).
                   07  HOLD-HORA                      PIC X(08).
                   07  HOLD-MSG                       PIC X(79).
                   07  HOLD-7090-SENHAS               PIC X(37).

                   07  HOLD-CURSOR.
                      09  HOLD-LINHA                  PIC 9(04) COMP.
                      09  HOLD-COLUNA                 PIC 9(04) COMP.

STEFAB**          X07  FILLER                         PIC X(334).
STEFAB             07  AREA-RUEC6250.
STEFAB                 09  IND-IBAMA                  PIC X(01).
STEFAB                 09  IND-UND-CONS               PIC X(01).
STEFAB                 09  IND-CONF-PUB               PIC X(01).
STEFAB                 09  IND-DESCRICAO              PIC X(300).
STEFA1                 09  IND-RURAL                  PIC X(01).
STEFA1                 09  IND-NIRF                   PIC X(01).
STEFA1                 09  IND-COUTRO                 PIC 9(02).
STEFA1                 09  IND-COUTRO-N               PIC X(01).
STEFA1                 09  FILLER                     PIC X(06).
220725             07  IND-TEM-NIRF                   PIC X(01).
220725             07  IND-TEM-CCIR                   PIC X(01).
180825             07  IND-TEM-IMOVEL                 PIC X(01).
180825             07  FILLER                         PIC X(017).


             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7100.

                   07  HOLD-7100-ITPO-GARNT           PIC X(40).
PW1577             07  HOLD-7100-RGARNT.
PW1577              09 HOLD-7100-RGARNT-1             PIC X(55).
PW1577              09 HOLD-7100-RGARNT-2             PIC X(55).
PW1577             07  HOLD-7100-QGARNT               PIC 9(11)V99.
PW1577             07  HOLD-7100-RQUALF-GARNT.
PW1577              09 HOLD-7100-RQUALF-GARNT-1       PIC X(50).
PW1577              09 HOLD-7100-RQUALF-GARNT-2       PIC X(50).
PW1577             07  HOLD-7100-RCOMPL-QUALF-GARNT.
PW1577              09 HOLD-7100-RCOMPL-QUALF-GARNT-1 PIC X(45).
PW1577              09 HOLD-7100-RCOMPL-QUALF-GARNT-2 PIC X(45).
PW1577             07  HOLD-7100-VGARNT               PIC 9(15)V99.
PW1577             07  HOLD-7100-ILOC-GARNT.
PW1577              09 HOLD-7100-ILOC-GARNT-1         PIC X(50).
PW1577              09 HOLD-7100-ILOC-GARNT-2         PIC X(50).
PW1577              09 HOLD-7100-ILOC-GARNT-3         PIC X(50).
PW1577              09 HOLD-7100-ILOC-GARNT-4         PIC X(50).
PW1577             07  HOLD-7100-CDMUN                PIC X(09).
PW1577             07  HOLD-7100-MUNIC                PIC X(30).
PW1577             07  HOLD-7100-CODUF                PIC X(02).
                   07  HOLD-7100-PERGUNTA             PIC X(01).
ACT019             07  HOLD-7100-CTERM                PIC X(08).
                   07  HOLD-7100-TIMESTAMP            PIC X(26).
ACT019             07  HOLD-7100-CTRBLOQ              PIC X(01).
ACT019             07  HOLD-7100-PAG                  PIC X(02).
ACT019             07  HOLD-7100-CEDULA               PIC X(09).
ACT019             07  HOLD-7100-FIMAMOS              PIC X(01).
ACT019             07  HOLD-7100-CSEQG                PIC 9(03).
ACT019             07  HOLD-7100-PERG2                PIC X(01).
                   07  HOLD-7100-MSG                  PIC X(79).
                   07  HOLD-7100-LINHA                PIC 9(04) COMP.
                   07  HOLD-7100-COLUNA               PIC 9(04) COMP.
POL097             07  FILLER                         PIC X(304).


             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7110.

                   07  HOLD-7110-OCORRENCIAS         OCCURS 12.
                      09  HOLD-7110-SELECIONA         PIC X(01).
                      09  HOLD-7110-DENOMINACAO       PIC X(25).
                      09  HOLD-7110-MATRICULA         PIC X(10).
POL097                09  HOLD-7110-ORIG-IMOV         PIC X(01).
                      09  HOLD-7110-MUNICIPIO         PIC X(15).
                      09  HOLD-7110-UF                PIC X(02).
                      09  HOLD-7110-STATUS            PIC X(21).

                   07  HOLD-7110-TIPO-GARANTIA        PIC X(20).
                   07  HOLD-7110-MSG                  PIC X(79).
POL097             07  HOLD-7110-CSEQ-PATRM-IMOV      PIC S9(05) COMP-3.
POL097             07  HOLD-7110-ORIG-IMOV-F9         PIC  X(01).
                   07  HOLD-7110-PAGINA               PIC  9(03) COMP-3.

POL097         05  FILLER                             PIC X(45).


             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7120.

                   07  HOLD-7120-IIMOV-RURAL          PIC X(40).
POL097             07  HOLD-7120-CMATR-IMOV           PIC X(10).
                   07  HOLD-7120-ELOGDR               PIC X(65).
                   07  HOLD-7120-MEXTEN               PIC 9(09)V99.
                   07  HOLD-7120-CUND-MEDD            PIC 9(03).
                   07  HOLD-7120-VIMOV                PIC 9(15)V99.
                   07  HOLD-7120-CGRAU                PIC 9(02).
                   07  HOLD-7120-IGRAU                PIC X(10).
                   07  HOLD-7120-TIT-DOMNI            PIC X(40).
                   07  HOLD-7120-DESCRT               PIC X(10).
POL097             07  HOLD-7120-CFL-LIVRO-REG-IMOV   PIC X(07).
                   07  HOLD-7120-CLIVRO-REG-IMOV      PIC X(04).
PW1577             07  HOLD-7120-CCARTR-IMOV-REG      PIC X(10).
PW1577             07  HOLD-7120-CGLEBA-IMOV-HIPOT    PIC X(10).
PW1577             07  HOLD-7120-CLOTE-IMOV-HIPOT     PIC X(10).
PW1577             07  HOLD-7120-ICARTR               PIC X(320).
ACT019             07  HOLD-7120-CDMUN                PIC X(09).
ACT019             07  HOLD-7120-MUNIC                PIC X(30).
ACT019             07  HOLD-7120-CODUF                PIC X(02).
                   07  HOLD-7120-IMUN                 PIC X(40).
PW1577             07  HOLD-7120-ALT-QUALIFIC         PIC X(01).
PW1577             07  HOLD-7120-EXISTE-TERC-PRO      PIC X(01).
PW1577             07  HOLD-7120-ESC-CPF-OUTORGA      PIC 9(09).
PW1577             07  HOLD-7120-NUM-CPF-OUTORGA      PIC 9(09).
PW1577             07  HOLD-7120-DIG-CPF-OUTORGA      PIC 9(02).
PW1577             07  HOLD-7120-IOUTORGA             PIC X(50).
POL097             07  HOLD-7120-CSEQ-PATRM-IMOV-X.
POL097                09  HOLD-7120-CSEQ-PATRM-IMOV   PIC 9(05).
POL097             07  HOLD-7120-ORIG-IMOV            PIC X(01).
                   07  HOLD-7120-INDICADOR            PIC X(01).
                   07  HOLD-7120-CSEQ-HIPOT-RURAL     PIC 9(02).

               05  HOLD-7120-TIMESTAMP                PIC X(26).
               05  HOLD-7120-ITPO-GARNT               PIC X(40).

               05  SAVE-7120-CONFIRMA-INCLUSAO        PIC X(01).


               05  SAVE-TELA-RUEC7130.

                   07  SAVE-7130-SELECT-MAX           PIC X(01).
                   07  SAVE-7130-CSEQ-HIPOT-RURAL-X.
                      09  SAVE-7130-CSEQ-HIPOT-RURAL  PIC 9(02).

POL097         05  FILLER                             PIC X(249).

             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7140.

                   07  HOLD-7140-TIMESTAMP            PIC X(26).
                   07  HOLD-7140-CTPO-GARTD           PIC 9(01).
                   07  HOLD-7140-AGENCIA              PIC 9(05).
                   07  HOLD-7140-CEDULA               PIC 9(09).
                   07  HOLD-7140-CGARTD               PIC 9(03).
                   07  HOLD-7140-IPSSOA               PIC X(60).
                   07  HOLD-7140-ISEGDR-PRPIA         PIC X(40).
                   07  HOLD-7140-CAPOLC-SEGUR         PIC X(15).
                   07  HOLD-7140-CTPO-EMBAL           PIC X(03).
                   07  HOLD-7140-SAFRA-INICIO         PIC 9(06).
                   07  HOLD-7140-SAFRA-FIM            PIC 9(06).
                   07  HOLD-7140-ITPO-EMBAL           PIC X(40).
                   07  HOLD-7140-QUND-PROD-PROVV      PIC 9(10).
                   07  HOLD-7140-CCLASF-PRODT-RURAL   PIC 9(05).
                   07  HOLD-7140-ICLASF-PRODT-RURAL   PIC X(40).
                   07  HOLD-7140-QCAPAC-PROD          PIC 9(11).
                   07  HOLD-7140-QUND-PROD-EFETV      PIC 9(11).
                   07  HOLD-7140-CUND-MEDD            PIC 9(03).
                   07  HOLD-7140-IUND-MEDD            PIC X(27).
                   07  HOLD-7140-CINDCD-PRODT-COOP    PIC X(01).
                   07  HOLD-7140-CCATEG-MUTUA         PIC 9(03).
                   07  HOLD-7140-ICATEG-MUTUA         PIC X(40).
                   07  HOLD-7140-VEGF                 PIC 9(11).

                   07  HOLD-7140-DATA                 PIC X(10).
                   07  HOLD-7140-HORA                 PIC X(08).
                   07  HOLD-7140-MSG                  PIC X(79).

                   07  HOLD-7140-CURSOR.
                      09  HOLD-7140-LINHA             PIC 9(04) COMP.
                      09  HOLD-7140-COLUNA            PIC 9(04) COMP.

                   07  HOLD-7140-CSEGDR               PIC 9(03).
POL097             07  FILLER                         PIC X(570).


             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC7150.
                   07  HOLD-7150-ITPO-GARNT           PIC X(40).
                   07  HOLD-7150-PAGINI               PIC X(02).
                   07  HOLD-7150-PAGFIM               PIC X(02).
                   07  HOLD-7150-RGARNT               PIC X(40).
                   07  HOLD-7150-QGARNT               PIC 9(12)V99.
MMF                07  HOLD-7150-QGARNT-X  REDEFINES
MMF                    HOLD-7150-QGARNT               PIC X(14).
                   07  HOLD-7150-RQUALF-GARNT         PIC X(20).
                   07  HOLD-7150-RCOMPL-QUALF-GARNT   PIC X(30).
                   07  HOLD-7150-VGARNT               PIC 9(16)V99.
                   07  HOLD-7150-IARZEM-PRODT-RURAL   PIC X(40).
                   07  HOLD-7150-CGC-CPF-ARMZ         PIC 9(9).
                   07  HOLD-7150-CFLIAL-CGC-ARMZ      PIC 9(5).
                   07  HOLD-7150-CCTRL-CPF-CGC-ARMZ   PIC 9(2).
                   07  HOLD-7150-CREG-CONAB-ARZEM     PIC 9(11).
                   07  HOLD-7150-CREG-CIBRAZEM        PIC 9(11).
                   07  HOLD-7150-IPPRIE-ARZEM-PRODT   PIC X(40).
                   07  HOLD-7150-RLOC-ARZEM-PRODT     PIC X(65).
ACT019             07  HOLD-7150-CDMUN                PIC X(09).
ACT019             07  HOLD-7150-MUNIC                PIC X(30).
ACT019             07  HOLD-7150-CODUF                PIC X(02).
                   07  HOLD-7150-CAP-EXP-GR           PIC 9(11)V99.
MMF                07  HOLD-7150-CAP-EXP-GR-X  REDEFINES
MMF                    HOLD-7150-CAP-EXP-GR           PIC X(13).
                   07  HOLD-7150-CAP-EXP-EN           PIC 9(11)V99.
MMF                07  HOLD-7150-CAP-EXP-EN-X  REDEFINES
MMF                    HOLD-7150-CAP-EXP-EN           PIC X(13).
                   07  HOLD-7150-IPSSOA               PIC X(60).
                   07  HOLD-7150-CGC-CPF              PIC 9(9).
                   07  HOLD-7150-CFLIAL-CGC           PIC 9(5).
                   07  HOLD-7150-CCTRL-CPF-CGC        PIC 9(2).
                   07  HOLD-7150-TIMESTAMP            PIC X(26).
                   07  HOLD-7150-SN-GARANTIA          PIC X(01).
                   07  HOLD-7150-SN-EXCLUSAO          PIC X(01).
                   07  HOLD-7150-PAG                  PIC 9(05).
                   07  HOLD-7150-FIM                  PIC X(01).
                   07  HOLD-7150-CSEQV09              PIC X(02).
                   07  HOLD-7150-CSEQV08              PIC X(03).
                   07  HOLD-7150-MSG                  PIC X(79).
                   07  HOLD-7150-CURSOR.
                      09  HOLD-7150-LINHA             PIC 9(04) COMP.
                      09  HOLD-7150-COLUNA            PIC 9(04) COMP.
                   07 HOLD-7150-CSEQ                  PIC 9(04) COMP.
                   07 HOLD-7150-PF                    PIC X.
                   07 HOLD-7150-SENHAS                PIC X(37).
                   07 HOLD-7150-VRTOT                 PIC 9(16)V99.
MMF                07 HOLD-7150-VRTOT-X  REDEFINES
MMF                   HOLD-7150-VRTOT                 PIC X(18).
                   07 HOLD-7150-AUXVG                 PIC 9(15)V99.
                   07 HOLD-7150-FASE                  PIC X.
                   07 HOLD-7150-CTRBLOQ               PIC X.
                   07 HOLD-7150-TERM                  PIC X(8).
                   07 HOLD-7150-INCLGAR               PIC X.
PW1577             07 HOLD-7150-QUALIFI               PIC X.
POL097             07  FILLER                         PIC X(349).

             03   FILLER                     REDEFINES AREA-SALVA-TELA1.

               05  HOLD-TELA-RUEC8600.
                   07  HOLD-8600-SENHA                PIC  X(37).
                   07  HOLD-8600-OPCAO                PIC  X(02).
                   07  HOLD-8600-DADOS.
                       09  HOLD-8600-AGENCIA-X.
                           11  HOLD-8600-AGENCIA      PIC  9(05).
                       09  HOLD-8600-CEDULA-X.
                           11  OUT-8600-CEDULA        PIC  9(09).
                       09  HOLD-8600-VIA01A           PIC  X(01).
                       09  HOLD-8600-VIA01B           PIC  X(01).
                       09  HOLD-8600-VIA01C           PIC  X(01).
                       09  HOLD-8600-VIA01D           PIC  X(01).
                       09  HOLD-8600-VIA01E           PIC  X(01).
                       09  HOLD-8600-VIA01F           PIC  X(01).
                       09  HOLD-8600-VIA01G           PIC  X(01).
CET                    09  HOLD-8600-VIA01H           PIC  X(01).
                       09  HOLD-8600-VIA04            PIC  X(01).
                       09  HOLD-8600-CONF1            PIC  X(21).
                       09  HOLD-8600-REIMPR           PIC  X(01).
                       09  HOLD-8600-CONF2            PIC  X(24).
                       09  HOLD-8600-MENSAG           PIC  X(79).

POL097             07  FILLER                         PIC  X(863).


B0923        03   FILLER                     REDEFINES AREA-SALVA-TELA1.
B0923
B0923          05  HOLD-TELA-RUEC7720.
B0923
B0923              07  HOLD-7720-CINDCD-CAR           PIC X(01).
B0923              07  FILLER                         PIC X(1049).


POL097       03   AREA-SALVA-TELA2                    PIC X(800).

             03   FILLER                     REDEFINES AREA-SALVA-TELA2.

               05  HOLD-TELA-8000.

                   07  HOLD-8000-PAGINA               PIC 9(02).
                   07  HOLD-8000-CONSISTE             PIC X(01).
                   07  HOLD-8000-CEDULA-RESTART       PIC 9(09).
                   07  HOLD-8000-AGENC-RESTART        PIC 9(04).
                   07  HOLD-8000-FASE                 PIC X(01).
                   07  HOLD-8000-LITAGE               PIC X(10).
                   07  HOLD-8000-CAGENC               PIC 9(05).
                   07  HOLD-8000-AGESC                PIC 9(05).
                   07  HOLD-8000-LITCN1               PIC X(10).
                   07  HOLD-8000-LITCN2               PIC X(01).
                   07  HOLD-8000-LITCN3               PIC X(01).
                   07  HOLD-8000-ATLZD                PIC X(01).

                   07  HOLD-8000-TABELA              OCCURS 10 TIMES.

                      09  HOLD-8000-ASTERA            PIC X(01).
                      09  HOLD-8000-AGENC             PIC 9(05).
                      09  HOLD-8000-CEDULA            PIC 9(09).
                      09  HOLD-8000-DTACED            PIC X(08).
PW2875                09  HOLD-8000-DTIMPR            PIC X(08).
PW2875                09  HOLD-8000-NOME              PIC X(09).
                      09  HOLD-8000-VALOR             PIC 9(08)V99.
PW2875                09  HOLD-8000-REST-STATUS       PIC 9(02).
                      09  HOLD-8000-SITU              PIC X(19).

POL097             07  FILLER                         PIC X(040).

             03   FILLER                     REDEFINES AREA-SALVA-TELA2.

               05  TABELA-INCONSIST           OCCURS  7.
                   07  TAB-CRSUMO-MSGEM-INCON         PIC X(05).
                   07  TAB-RMSGEM-INCON               PIC X(39).
                   07  TAB-RESP-APROVACAO             PIC 9(01).

POL097         05  HOLD-TELA-RUEC7000                 PIC X(485).

PW1577       03   FILLER                     REDEFINES AREA-SALVA-TELA2.
PW1577         05  OCORRENCIAS-ENVOLVIDOS-BL1 OCCURS  7.
PW1577             07  CGCCPF-ENVOL                   PIC 9(09) COMP-3.
PW1577             07  FILIAL-ENVOL                   PIC 9(04).
PW1577             07  CTRL-CPF-ENVOL                 PIC 9(02).
PW1577             07  CSEQ-PSSOA-ENVOL               PIC 9(03).
PW1577             07  ESTOQUE-ENVOL                  PIC X(01).
PW1577             07  NOME-ENVOL                     PIC X(60).
PW1577             07  COD-PSSOA-ENVOL                PIC X(01).
PW1577             07  SITUACAO-ENVOL                 PIC X(11).
PW1577             07  QUALIFIC-CONJ                  PIC X(01).
PW1577             07  QUALIFIC-ENVOL                 PIC X(01).
PW1577             07  GRUPO1-ATU-ENVOL               PIC 9(05).
PW1577             07  GRUPO2-ATU-ENVOL               PIC 9(05).
PW1577             07  GRUPO3-ATU-ENVOL               PIC 9(05).
PW1577             07  CONTIN-ATU-ENVOL               PIC X(01).
PW1577             07  EXISTE-ATU-ENVOL               PIC X(01).
PW1577         05  CCDULA-ENVOL                       PIC 9(09).
PW1577         05  MAIS-AVAL-ENVOL                    PIC X(01).
PW1577         05  TSUTIL-ENVOL                       PIC X(26).
PW1577         05  FASE-ENVOL                         PIC X(01).
PW1577         05  PAG-ENVOL                          PIC 9(03).
PW1577         05  FIMAMO-ENVOL                       PIC X(01).
PW1577         05  TERM-ENVOL                         PIC X(08).
PW1577         05  AGENCIA-ENVOL                      PIC X(05).
PW1577         05  IMPED-ENVOL                        PIC X(01).
PW1577         05  IMPED-23-ENVOL                     PIC X(01).
PW1577         05  BLOQ-ENVOL                         PIC X(01).
PW1577         05  CODBLOQ-ENVOL                      PIC X(01).

           02  FILLER                         REDEFINES    AREA-TOTAL.

               05  HOLD-TELA-RUECT703                 PIC X(834).

               05  HOLD-TAB-T703                      OCCURS 11.
                   07  HOLD-VLFIN-T703                PIC 9(12)V99.
                   07  HOLD-VLREC-T703                PIC 9(12)V99.

               05  HOLD-VLFIN-TOTAL                   PIC 9(16)V99.
               05  HOLD-VLREC-TOTAL                   PIC 9(16)V99.
               05  HOLD-DATA-LIBERACAO                PIC X(10).

               05  FILLER                             PIC X(2212).

           02  FILLER                         REDEFINES    AREA-TOTAL.

               05  HOLD-TELA-RUECT704.
                   07  HOLD-CEDULA-T704               PIC X(09).
                   07  HOLD-TIMEST-T704               PIC X(26).
                   07  HOLD-PAG-T704                  PIC 9(01).
                   07  HOLD-TOTOT-T704-R.
                      09  HOLD-TOTOT-T704             PIC 9(12)V99.
                   07  HOLD-TOFIN-T704-R.
                      09  HOLD-TOFIN-T704             PIC 9(12)V99.
                   07  HOLD-TOREC-T704-R.
                      09  HOLD-TOREC-T704             PIC 9(12)V99.
                   07  HOLD-OPCONS-T704               PIC X(01).
                   07  HOLD-MDOLAR-T704               PIC X(46).
                   07  HOLD-OPCONF-T704               PIC X(01).
                   07  HOLD-OPOBS-T704                PIC X(01).
                   07  HOLD-LIBFIN-T704               PIC X(01).
                   07  HOLD-ESTEGF-T704               PIC X(01).
               05  HOLD-VLFIN-TOTAL-T704              PIC 9(16)V99.
               05  HOLD-VLREC-TOTAL-T704              PIC 9(16)V99.
               05  HOLD-DATA-LIBERACAO-T704           PIC X(10).

               05  HOLD-TABELA-RUECT704.
                   07  HOLD-TABELA-T704               OCCURS  3.
                      09  HOLD-TAB-T704               OCCURS  9.
                          11  HOLD-ITEM-T704          PIC X(15).
                          11  HOLD-DTPAR-T704         PIC X(10).
EG0517                    11  HOLD-DTFIM-T704         PIC X(10).
                          11  HOLD-VLTOT-T704-R.
                              13  HOLD-VLTOT-T704     PIC 9(12)V99.
                          11  HOLD-VLFIN-T704-R.
                              13  HOLD-VLFIN-T704     PIC 9(12)V99.
                          11  HOLD-VLREC-T704-R.
                              13  HOLD-VLREC-T704     PIC 9(12)V99.
                          11  HOLD-OPLIB-T704         PIC X(01).
                          11  HOLD-OPIME-T704         PIC X(01).
EG0517         05  FILLER                             PIC X(1092).

      *-----------------------------------------------------------------
      * BOOK DE COMUNICACO COM PROGRAMA RUEC7200
      * TELA RUECT720
      *-----------------------------------------------------------------

           02  FILLER                         REDEFINES    AREA-TOTAL.

               05 HOLD-7200-CEDULA             PIC  9(09).
               05 HOLD-7200-NAVEGA             PIC  X(37).
               05 HOLD-7200-FASE               PIC  9(01).
               05 HOLD-7200-CTERM              PIC  X(08).
               05 HOLD-7200-TIMES              PIC  X(26).
               05 HOLD-7200-AGENC              PIC  9(05).
               05 HOLD-7200-OPCAO              PIC  9(02).
               05 HOLD-7200-LINHA  OCCURS   11 TIMES.
                  07 HOLD-7200-BANCO-ATR       PIC S9(04) COMP.
                  07 HOLD-7200-BANCO-ALF.
                     09 HOLD-7200-BANCO        PIC  9(03).
ACT137            07 HOLD-7200-CEDUL-ATR       PIC S9(04) COMP.
ACT137            07 HOLD-7200-CEDUL-ALF       PIC  X(17).
ACT137            07 HOLD-7200-FINAL-ATR       PIC S9(04) COMP.
ACT137            07 HOLD-7200-FINAL-ALF       PIC  X(20).
ACT137            07 HOLD-7200-DD-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-7200-DD-DCDULA-ALF.
ACT137               09 HOLD-7200-DD-DCDULA    PIC  9(02).
ACT137            07 HOLD-7200-MM-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-7200-MM-DCDULA-ALF.
ACT137               09 HOLD-7200-MM-DCDULA    PIC  9(02).
ACT137            07 HOLD-7200-AA-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-7200-AA-DCDULA-ALF.
ACT137               09 HOLD-7200-AA-DCDULA    PIC  9(04).
ACT137            07 HOLD-7200-DD-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-7200-DD-DVCTO-ALF.
ACT137               09 HOLD-7200-DD-DVCTO     PIC  9(02).
ACT137            07 HOLD-7200-MM-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-7200-MM-DVCTO-ALF.
ACT137               09 HOLD-7200-MM-DVCTO     PIC  9(02).
ACT137            07 HOLD-7200-AA-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-7200-AA-DVCTO-ALF.
ACT137               09 HOLD-7200-AA-DVCTO     PIC  9(04).
                  07 HOLD-7200-VALOR-ATR       PIC S9(04) COMP.
                  07 HOLD-7200-VALOR-ALF.
                     09 HOLD-7200-VALOR        PIC  ZZZ.ZZZ.ZZZ,ZZ.
                  07 HOLD-7200-CSEQ            PIC  9(03).
ACT137         05 HOLD-7200-BANCOS-ALF.
ACT137            07 HOLD-7200-BANCOS          PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137         05 HOLD-7200-BRADES-ALF.
ACT137            07 HOLD-7200-BRADES          PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137         05 HOLD-7200-TOTAL-ALF.
ACT137            07 HOLD-7200-TOTAL           PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05 HOLD-7200-PAGATU             PIC  9(03).
               05 HOLD-7200-PAGFIM             PIC  9(03).
               05 HOLD-7200-LITERAL            PIC  X(38).
               05 HOLD-7200-RESPTA             PIC  X(01).
               05 HOLD-7200-LIN                PIC S9(04) COMP.
               05 HOLD-7200-COL                PIC S9(04) COMP.
               05 HOLD-7200-IND-LIN            PIC  9(02).
               05 HOLD-7200-MENSAGEM           PIC  X(79).

ACT137         05  FILLER                      PIC  X(2099).
      *-----------------------------------------------------------------
      *               FIM BOOK  RUEC7200
      *
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      * BOOK DE COMUNICACO COM PROGRAMA RUEC8200
      * TELA RUECT820
      *-----------------------------------------------------------------

           02  FILLER                         REDEFINES    AREA-TOTAL.

               05 HOLD-8200-CEDULA             PIC  9(09).
               05 HOLD-8200-NAVEGA             PIC  X(37).
               05 HOLD-8200-FASE               PIC  9(01).
               05 HOLD-8200-CTERM              PIC  X(08).
               05 HOLD-8200-TIMES              PIC  X(26).
               05 HOLD-8200-AGENC              PIC  9(05).
               05 HOLD-8200-OPCAO              PIC  9(02).
               05 HOLD-8200-LINHA  OCCURS   11 TIMES.
                  07 HOLD-8200-BANCO-ATR       PIC S9(04) COMP.
                  07 HOLD-8200-BANCO-ALF.
                     09 HOLD-8200-BANCO        PIC  9(03).
ACT137            07 HOLD-8200-CEDUL-ATR       PIC S9(04) COMP.
ACT137            07 HOLD-8200-CEDUL-ALF       PIC  X(17).
ACT137            07 HOLD-8200-FINAL-ATR       PIC S9(04) COMP.
ACT137            07 HOLD-8200-FINAL-ALF       PIC  X(20).
ACT137            07 HOLD-8200-DD-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-8200-DD-DCDULA-ALF.
ACT137               09 HOLD-8200-DD-DCDULA    PIC  9(02).
ACT137            07 HOLD-8200-MM-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-8200-MM-DCDULA-ALF.
ACT137               09 HOLD-8200-MM-DCDULA    PIC  9(02).
ACT137            07 HOLD-8200-AA-DCDULA-ATR   PIC S9(04) COMP.
ACT137            07 HOLD-8200-AA-DCDULA-ALF.
ACT137               09 HOLD-8200-AA-DCDULA    PIC  9(04).
ACT137            07 HOLD-8200-DD-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-8200-DD-DVCTO-ALF.
ACT137               09 HOLD-8200-DD-DVCTO     PIC  9(02).
ACT137            07 HOLD-8200-MM-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-8200-MM-DVCTO-ALF.
ACT137               09 HOLD-8200-MM-DVCTO     PIC  9(02).
ACT137            07 HOLD-8200-AA-DVCTO-ATR    PIC S9(04) COMP.
ACT137            07 HOLD-8200-AA-DVCTO-ALF.
ACT137               09 HOLD-8200-AA-DVCTO     PIC  9(04).
                  07 HOLD-8200-VALOR-ATR       PIC S9(04) COMP.
                  07 HOLD-8200-VALOR-ALF.
                     09 HOLD-8200-VALOR        PIC  ZZZ.ZZZ.ZZZ,ZZ.
                  07 HOLD-8200-CSEQ            PIC  9(03).
ACT137         05 HOLD-8200-BANCOS-ALF.
ACT137            07 HOLD-8200-BANCOS          PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137         05 HOLD-8200-BRADES-ALF.
ACT137            07 HOLD-8200-BRADES          PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137         05 HOLD-8200-TOTAL-ALF.
ACT137            07 HOLD-8200-TOTAL           PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05 HOLD-8200-PAGATU             PIC  9(03).
               05 HOLD-8200-PAGFIM             PIC  9(03).
               05 HOLD-8200-LITERAL            PIC  X(38).
               05 HOLD-8200-RESPTA             PIC  X(01).
               05 HOLD-8200-LIN                PIC S9(04) COMP.
               05 HOLD-8200-COL                PIC S9(04) COMP.
               05 HOLD-8200-IND-LIN            PIC  9(02).
               05 HOLD-8200-MENSAGEM           PIC  X(79).

           05 FILLER                       PIC  X(0177).
           05 HOLD-8200G-LINHA  OCCURS  11 TIMES.
              07 HOLD-8200G-BANCO-ATR      PIC S9(04) COMP.
              07 HOLD-8200G-BANCO-ALF.
                 09 HOLD-8200G-BANCO       PIC  9(03).
              07 HOLD-8200G-CEDUL-ATR      PIC S9(04) COMP.
              07 HOLD-8200G-CEDUL-ALF      PIC  X(17).
              07 HOLD-8200G-FINAL-ATR      PIC S9(04) COMP.
              07 HOLD-8200G-FINAL-ALF      PIC  X(20).
              07 HOLD-8200G-DD-DCDULA-ATR  PIC S9(04) COMP.
              07 HOLD-8200G-DD-DCDULA-ALF.
                 09 HOLD-8200G-DD-DCDULA   PIC  9(02).
              07 HOLD-8200G-MM-DCDULA-ATR  PIC S9(04) COMP.
              07 HOLD-8200G-MM-DCDULA-ALF.
                 09 HOLD-8200G-MM-DCDULA   PIC  9(02).
              07 HOLD-8200G-AA-DCDULA-ATR  PIC S9(04) COMP.
              07 HOLD-8200G-AA-DCDULA-ALF.
                 09 HOLD-8200G-AA-DCDULA   PIC  9(04).
              07 HOLD-8200G-DD-DVCTO-ATR   PIC S9(04) COMP.
              07 HOLD-8200G-DD-DVCTO-ALF.
                 09 HOLD-8200G-DD-DVCTO    PIC  9(02).
              07 HOLD-8200G-MM-DVCTO-ATR   PIC S9(04) COMP.
              07 HOLD-8200G-MM-DVCTO-ALF.
                 09 HOLD-8200G-MM-DVCTO    PIC  9(02).
              07 HOLD-8200G-AA-DVCTO-ATR   PIC S9(04) COMP.
              07 HOLD-8200G-AA-DVCTO-ALF.
                 09 HOLD-8200G-AA-DVCTO    PIC  9(04).
              07 HOLD-8200G-VALOR-ATR      PIC S9(04) COMP.
              07 HOLD-8200G-VALOR-ALF.
                 09 HOLD-8200G-VALOR       PIC  X(14).
              07 HOLD-8200G-CSEQ           PIC  9(03).
           05  FILLER                      PIC  X(0899).

      *-----------------------------------------------------------------
      * BOOK DE COMUNICACO COM PROGRAMA RUEC7400
      * TELA RUECT740
      *-----------------------------------------------------------------

           02  FILLER                         REDEFINES    AREA-TOTAL.
             03 HOLD-7400.
               05 HOLD-7400-CCONTR             PIC  9(09).
               05 HOLD-7400-DIGCON             PIC  9(01).
               05 HOLD-7400-CDAGE              PIC  9(05).
               05 HOLD-7400-NMAGE              PIC  X(20).
               05 HOLD-7400-CCDULA             PIC  9(09).
               05 HOLD-7400-CNPJ               PIC  9(09).
               05 HOLD-7400-FILIAL             PIC  9(04).
               05 HOLD-7400-DIGCNPJ            PIC  9(02).
               05 HOLD-7400-VFINAN             PIC  9(12)V99.
               05 HOLD-7400-NMMUTU             PIC  X(40).
               05 HOLD-7400-OCULTO             PIC  X(48).
               05 HOLD-7400-CDCART             PIC  X(10).
               05 HOLD-7400-NMCART             PIC  X(320).
               05 HOLD-7400-CDMATRIC           PIC  X(10).
               05 HOLD-7400-CDMUN              PIC  9(09).
               05 HOLD-7400-NMMUN              PIC  X(40).
               05 HOLD-7400-REGHIPO            PIC  X(10).
               05 HOLD-7400-DTRHIPO            PIC  9(08).
               05 HOLD-7400-BXRHIPO            PIC  X(01).
               05 HOLD-7400-REGPENH            PIC  X(10).
               05 HOLD-7400-DTRPENH            PIC  9(08).
               05 HOLD-7400-BXRPENH            PIC  X(01).
               05 HOLD-7400-TRANANT            PIC  X(08).
               05 FILLER                       PIC  X(1904).

      * **** NAO ALTERAR CAMPO ABAIXO POIS EH UTILIZADO NO RUEC2000,
      * ****    QDO NAVEGA DA OPCAO 03 P/ INCLUSAO CARTORIO - RUEC7400.
      * **** RUEC2000 MOVE DADOS PARA HOLD-TELA-8000 (900 POS.FINAIS) E
      * ****    AREA-COMUM (600 POS. INICIAIS DO REGISTRO).

             03 HOLD-2000                      PIC  X(900).

POL097*-----------------------------------------------------------------
POL097* BOOK DE COMUNICACAO COM PGMS DO CAD. RURAL - OPCAO 12 RUEC9172
POL097*
POL097*-----------------------------------------------------------------

POL097     02  HOLD-9172-REG                   REDEFINES    AREA-TOTAL
POL097                                         PIC  X(3400).
POL097*
POL097****  DETALHES ESTAO DESCRITOS NA INC I#RUEC28, DENTRO DO
POL097****  ITEM DE GRUPO R28-AREA-TOTAL-R7734.

POL097*-----------------------------------------------------------------
POL097*               FIM BOOK  RUEC9172
POL097*
POL097*-----------------------------------------------------------------
      *================================================================*
B0923 * ANALISTA REPONSAVEL: BELGA JUNIOR - FOURSYS                    *
B0923 * DATA...............: SETEMBRO - 2023                           *
B0923 * OBJETIVOS..........: RESOLUCAO 5.078 - REDUTOR DE TAXA - CAR   *
      *================================================================*
