      ******************************************************************
      * NOME BOOK : GRUPW01S                                           *
      * DESCRICAO : OBTER O GRUPO ATRAVES DO CNPJ                      *
      * DATA      : 23/10/2025                                         *
      * ANALISTA  : TALASSA IOSHIMOTO                                  *
      * MODULO    : GRUP201C                                           *
      * OBS       : FORMATA  AS DATAS COM PONTOS  EX: 'DD.MM.AAAA'     *
      *             O MODULO NAO CHAMA POOL7100;  ERRO-AREA E SQLCA    *
      *             SAO RETORNADOS NO FINAL DA INC E CADA CHAMADOR     *
      *             DEVERA TRATAR O RETORNO.                           *
      * TAMANHO   : 542 BYTES                                          *
      ******************************************************************
       01  GRUPW01S-REG-EMPRESA.
           03  GRUPW01S-AREA-ENVIO-CHAMADOR.
               05  GRUPW01S-PGM-CHAMADOR              PIC X(08).
               05  GRUPW01S-CGC-CPF                   PIC X(11).
               05  GRUPW01S-FILIAL-CGC-CPF            PIC X(04).
               05  GRUPW01S-CTRL-CGC-CPF              PIC 9(02).
               05  GRUPW01S-NUMERO-GRUPO-ENV          PIC 9(09).
      *
      *      NUMERO GRUPO EH OPCIONAL; SE INFORMADO, O MODULO RETORNA
      *      DADOS DA EMPRESA PERTENCETE AO GRUPO, INDEPENDENTE DA
      *      SITUACAO/TIPO DO GRUPO; SE NAO FOR INFORMADO, O MODULO
      *      RETORNA O GRUPO OFICIAL/AUTOMATICO A QUAL PERTENCE O CNPJ
      *      SOMENTE NA SITUACAO DE GRUPO ATIVO
      *
               05  FILLER                             PIC X(10).

           03  GRUPW01S-AREA-RETORNO.
               05  GRUPW01S-NUMERO-GRUPO-RET          PIC 9(09).
               05  GRUPW01S-DADOS-EMPRESA-GP34.
                   07  GRUPW01S-NOME-EMPRESA          PIC X(40).
                   07  GRUPW01S-COD-USUARIO-SENHA     PIC X(07).
                   07  GRUPW01S-DATA-ULT-ALTER-REG    PIC X(10).
                   07  GRUPW01S-TIPO-PESSOA           PIC X(01).
      *                F   -  FISICA
      *                J   -  JIRIDICA
                   07  GRUPW01S-ISENC-CGC-CPF         PIC X(01).
      *                S   -  SIM
      *                N   -  NAO
                   07  GRUPW01S-SUBTPO-GPOEMPR-ECONM  PIC 9(03) COMP-3.
      *                001 -  CC - CENTRALIZADO
      *                002 -  CO - OPERACIONAL
      *                003 -  US - USINAS
      *                005 -  CF - CAFE
      *                006 -  MC - MIDLE
      *                007 -  VC - VAREJO CENTRALIZADO
      *                008 -  MO -
      *                009 -  JD - JAPAN DESK
      *                010 -  RB -
      *                011 -  ED - EURO DESK
      *                012 -  RC - REGIONAL CENTRALIZADO
      *                013 -  RO - REGIONAL OPERACIONAL
                   07  GRUPW01S-DATA-INCLUSAO         PIC X(10).
                   07  GRUPW01S-INCOR-EMPRESA         PIC 9(01) COMP-3.
                   07  GRUPW01S-DATA-INCOR-EMPRESA    PIC X(10).
                   07  GRUPW01S-DS-FONTE-INFOR-INCOR  PIC X(30).
                   07  GRUPW01S-CGC-ICORPORADORA      PIC X(11).
                   07  GRUPW01S-FILIAL-CGC-INCORP     PIC X(04).
                   07  GRUPW01S-CTRL-CGC-INCORP       PIC 9(02) COMP-3.
                   07  GRUPW01S-IDENT-PESSOA          PIC 9(18) COMP-3.
                   07  GRUPW01S-CTRL-IDENT-PESSOA     PIC 9(02) COMP-3.
                   07  GRUPW01S-HORA-ULT-ALTER-REG    PIC X(08).
                   07  GRUPW01S-IDENTIFICADOR-MIDIA   PIC X(08).
               05  GRUPW01S-DADOS-GRUPO.
                   07  GRUPW01S-TIPO-GRUPO            PIC 9(03) COMP-3.
      *                001 -  NORMAL
      *                002 -  AUTOMATICO
      *                003 -  FAMILIAR
      *                004 -  CORPORATE
      *                005 -  ISOLADO
      *                006 -  ESPECIAL
      *                007 -  PESSOA FISICA
      *                008 -  PREFEITURAS
      *                009 -  GOVERNO ESTADUAL
      *                010 -  GOVERNO FEDERAL
                   07  GRUPW01S-SIT-GRUPO             PIC 9(03) COMP-3.
      *                001 -  EM FORMALIZACAO
      *                002 -  ATIVO
      *                003 -  EXCLUSIVO DE CREDITO
      *                004 -  HISTORICO
      *                005 -  BLOQUEADO
                   07  GRUPW01S-SUBTPO-GRUPO          PIC 9(03) COMP-3.
      *                001 -  CC - CENTRALIZADO
      *                002 -  CO - OPERACIONAL
      *                003 -  US - USINAS
      *                005 -  CF - CAFE
      *                006 -  MC - MIDLE
      *                007 -  VC - VAREJO CENTRALIZADO
      *                008 -  MO -
      *                009 -  JD - JAPAN DESK
      *                010 -  RB -
      *                011 -  ED - EURO DESK
      *                012 -  RC - REGIONAL CENTRALIZADO
      *                013 -  RO - REGIONAL OPERACIONAL
                   07  GRUPW01S-NOME-GRUPO            PIC X(30).
                   07  GRUPW01S-RETURN-CODE           PIC 9(03).
      *                000 -  OK
      *                001 -  ARGUMENTO PESQUISA NAO ENCONTRADO
      *                004 -  INCONSIST. NOS DADOS ENVIADOS P/ CHAMADOR
      *                999 -  ERRO DB2
                   07  GRUPW01S-DATA-VALID-GRUPO      PIC X(10).
                   07  FILLER                         PIC X(40).
               05  GRUPW01S-ERRO-AREA                 PIC X(107).
               05  GRUPW01S-SQLCA                     PIC X(136).
