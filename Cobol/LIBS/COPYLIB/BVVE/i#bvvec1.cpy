      *================================================================*
      *       AFILIACAO ELETRONICA DE ESTABELECIMENTOS - CIELO         *
      *                                                                *
      * BASE PARA IMPRESSAO DOS RELATORIOS DA AFILIACAO                *
      * LREL: 900                                                      *
      *                                                                *
      *================================================================*
       01  BVVEC1-AREA-ARQRELAT.
           05  BVVEC1-CNPJ-CPF             PIC 9(009)      VALUE ZEROS.
           05  BVVEC1-FILIAL               PIC 9(005)      VALUE ZEROS.
           05  BVVEC1-CTRL                 PIC 9(002)      VALUE ZEROS.
           05  BVVEC1-RAZAO-SOCIAL         PIC X(032)      VALUE SPACES.
           05  BVVEC1-INSC-ESTAD           PIC X(015)      VALUE SPACES.
           05  BVVEC1-NOME-FANT            PIC X(032)      VALUE SPACES.
           05  BVVEC1-COD-RAMO             PIC 9(005)      VALUE ZEROS.
           05  BVVEC1-DSC-RAMO             PIC X(040)      VALUE SPACES.
           05  BVVEC1-TP-EQPTO             PIC X(006)      VALUE SPACES.
           05  BVVEC1-DT-CADASTRO          PIC X(010)      VALUE SPACES.
           05  BVVEC1-COD-AGENCIA          PIC 9(005)      VALUE ZEROS.
           05  BVVEC1-DSC-AGENCIA          PIC X(050)      VALUE SPACES.
           05  BVVEC1-CONTA                PIC 9(013)      VALUE ZEROS.
           05  BVVEC1-CONTA-DIG            PIC 9(001)      VALUE ZEROS.
           05  BVVEC1-CFUNC-BRAD           PIC 9(009)      VALUE ZEROS.
           05  BVVEC1-MICRO-EMPR           PIC X(003)      VALUE SPACES.
           05  BVVEC1-NRO-ESTABL           PIC 9(010)      VALUE ZEROS.
           05  BVVEC1-EMAIL                PIC X(050)      VALUE SPACES.
           05  BVVEC1-CONTATO              PIC X(032)      VALUE SPACES.
      * DADOS ENDERECO DO ESTABELECIMENTO -----------------------------*
           05  BVVEC1-EE-CEP               PIC 9(005)      VALUE ZEROS.
           05  BVVEC1-EE-CEP-COMPL         PIC 9(003)      VALUE ZEROS.
           05  BVVEC1-EE-UF                PIC X(002)      VALUE SPACES.
           05  BVVEC1-EE-CIDADE            PIC X(028)      VALUE SPACES.
           05  BVVEC1-EE-ENDERECO          PIC X(064)      VALUE SPACES.
      * DADOS ENDERECO DE CORRESPONDENCIA -----------------------------*
           05  BVVEC1-EC-CEP               PIC 9(005)      VALUE ZEROS.
           05  BVVEC1-EC-CEP-COMPL         PIC 9(003)      VALUE ZEROS.
           05  BVVEC1-EC-UF                PIC X(002)      VALUE SPACES.
           05  BVVEC1-EC-CIDADE            PIC X(028)      VALUE SPACES.
           05  BVVEC1-EC-ENDERECO          PIC X(064)      VALUE SPACES.
      * DADOS DE TELEFONES --------------------------------------------*
           05  BVVEC1-FIXO-DDD             PIC X(004)      VALUE SPACES.
           05  BVVEC1-FIXO-NUMERO-8        PIC 9(008)      VALUE ZEROS.
           05  BVVEC1-FIXO-NUMERO-11       PIC 9(011)      VALUE ZEROS.
           05  BVVEC1-CEL-DDD              PIC X(004)      VALUE SPACES.
           05  BVVEC1-CEL-NUMERO-8         PIC 9(008)      VALUE ZEROS.
           05  BVVEC1-CEL-NUMERO-11        PIC 9(011)      VALUE ZEROS.
      * DADOS DE PROPRIETARIOS ----------------------------------------*
           05  BVVEC1-PROPRIETARIOS    OCCURS 3 TIMES.
               07 BVVEC1-PROP-CPF          PIC 9(009)      VALUE ZEROS.
               07 BVVEC1-PROP-CTRL         PIC 9(002)      VALUE ZEROS.
               07 BVVEC1-PROP-NOME         PIC X(032)      VALUE SPACES.
               07 BVVEC1-PROP-DT-NASC      PIC X(010)      VALUE SPACES.
           05 FILLER                       PIC X(162)      VALUE SPACES.
