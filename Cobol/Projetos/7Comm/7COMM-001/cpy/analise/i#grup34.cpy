      **********************************************************
      *   INC   I#GRUP34 DO MODULO GRUP0930                    *
      *                                                        *
      *   INC UTILIZADA PARA OBTER O GRUPO ATRAVES DO CNPJ     *
      *                                                        *
      *   OBS: FORMATA  AS DATAS COM PONTOS  EX: 'DD.MM.AAAA'  *
      *        O MODULO NAO CHAMA POOL7100;  ERRO-AREA E SQLCA *
      *        SAO RETORNADOS NO FINAL DA INC E CADA CHAMADOR  *
      *        DEVERA TRATAR O RETORNO.                        *
      *                                                        *
      **********************************************************
       01  REG-EMPRESA-GRUPO-GP34.
           03  AREA-ENVIO-CHAMADOR-GP34.
               05  PGM-CHAMADOR-GP34               PIC X(08).
               05  CGC-CPF-GRUPO-GP34              PIC 9(11).
               05  FILIAL-CGC-CPF-GRUPO-GP34       PIC 9(04).
               05  CTRL-CGC-CPF-GRUPO-GP34         PIC 9(02).
               05  NUMERO-GRUPO-ENV-GP34           PIC 9(09).
      *
      *      NUMERO GRUPO EH OPCIONAL; SE INFORMADO, O MODULO RETORNA
      *      DADOS DA EMPRESA PERTENCETE AO GRUPO, INDEPENDENTE DA
      *      SITUACAO/TIPO DO GRUPO; SE NAO FOR INFORMADO, O MODULO
      *      RETORNA O GRUPO OFICIAL/AUTOMATICO A QUAL PERTENCE O CNPJ
      *      SOMENTE NA SITUACAO DE GRUPO ATIVO
      *
               05  FILLER                          PIC X(10).

           03  AREA-RETORNO-CHAMADO-GP34.
               05  NUMERO-GRUPO-RET-GP34           PIC 9(09).
               05  DADOS-EMPRESA-GP34.
                   07  NOME-EMPRESA-GP34           PIC X(40).
                   07  COD-USUARIO-SENHA-GP34      PIC X(07).
                   07  DATA-ULT-ALTER-REG-GP34     PIC X(10).
                   07  TIPO-PESSOA-GP34            PIC X(01).
      *                F   -  FISICA
      *                J   -  JIRIDICA
                   07  ISENC-CGC-CPF-GP34          PIC X(01).
      *                S   -  SIM
      *                N   -  NAO
                   07  SUBTPO-GPOEMPR-ECONM-GP34   PIC 9(03) COMP-3.
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
                   07  DATA-INCLUSAO-GP34          PIC X(10).
                   07  INCOR-EMPRESA-GP34          PIC 9(01) COMP-3.
                   07  DATA-INCOR-EMPRESA-GP34     PIC X(10).
                   07  DS-FONTE-INFOR-INCOR-GP34   PIC X(30).
                   07  CGC-ICORPORADORA-GP34       PIC 9(11) COMP-3.
                   07  FILIAL-CGC-INCORP-GP34      PIC 9(04) COMP-3.
                   07  CTRL-CGC-INCORP-GP34        PIC 9(02) COMP-3.
                   07  IDENT-PESSOA-GP34           PIC 9(18) COMP-3.
                   07  CTRL-IDENT-PESSOA-GP34      PIC 9(02) COMP-3.
                   07  HORA-ULT-ALTER-REG-GP34     PIC X(08).
                   07  IDENTIFICADOR-MIDIA-GP34    PIC X(08).
               05  DADOS-GRUPO-GP34.
                   07  TIPO-GRUPO-GP34             PIC 9(03) COMP-3.
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
                   07  SIT-GRUPO-GP34              PIC 9(03) COMP-3.
      *                001 -  EM FORMALIZACAO
      *                002 -  ATIVO
      *                003 -  EXCLUSIVO DE CREDITO
      *                004 -  HISTORICO
      *                005 -  BLOQUEADO
                   07  SUBTPO-GRUPO-GP34           PIC 9(03) COMP-3.
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
                   07  NOME-GRUPO-GP34             PIC X(30).
                   07  RETURN-CODE-GP34            PIC 9(03).
      *                000 -  OK
      *                001 -  ARGUMENTO PESQUISA NAO ENCONTRADO
      *                004 -  INCONSIST. NOS DADOS ENVIADOS P/ CHAMADOR
      *                999 -  ERRO DB2
                   07  DATA-VALID-GRUPO-GP34       PIC X(10).
                   07  FILLER                      PIC X(40).
               05  ERRO-AREA-GP34                  PIC X(107).
               05  SQLCA-GP34                      PIC X(136).
