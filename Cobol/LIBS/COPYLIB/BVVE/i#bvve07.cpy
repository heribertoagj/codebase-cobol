      *** I#BVVE07 *****************************************************
      *                   SISTEMA : AFILIACAO ELETRONICA               *
      *                                                                *
      *       ARQUIVO : PROPOSTA DE AFILIACAO RECEBIDO DA VAN          *
      *                                                                *
      *                   RECORD SIZE = 1.000                          *
      ******************************************************************
       01  AE-VAN-REGISTRO.
           03  AE-VAN-HEADER.
               05  AE-VAN-H-IDENT-LINHA            PIC  9(001).
               05  AE-VAN-H-NOME-ARQ               PIC  X(025).
               05  AE-VAN-H-SEQ-REMESSA            PIC  9(011).
               05  AE-VAN-H-DT-GERACAO             PIC  X(008).
               05  AE-VAN-H-HR-GERACAO             PIC  X(006).
               05  FILLER                          PIC  X(949).

           03  AE-VAN-DETALHE  REDEFINES  AE-VAN-HEADER.
               05  AE-VAN-IDENT-LINHA              PIC  9(001).
               05  AE-VAN-COD-PARCEIRO             PIC  9(002).
                   88  AE-VAN-CIELO                           VALUE 04.
                   88  AE-VAN-GETNET                          VALUE 20.
                   88  AE-VAN-ELAVON                          VALUE 21.
                   88  AE-VAN-REDE                            VALUE 22.
                   88  AE-VAN-FIRST                           VALUE 23.
                   88  AE-VAN-GLOBAL-PAYMENT                  VALUE 24.
                   88  AE-VAN-STONE                           VALUE 25.
               05  AE-VAN-COD-GRUPO                PIC  9(002).
               05  AE-VAN-COD-AFILIADOR            PIC  9(009).
               05  AE-VAN-TIPO-EC-VAN              PIC  X(001).
               05  AE-VAN-TIPO-PESSOA              PIC  X(001).
               05  AE-VAN-EC-GLOBAL                PIC  X(001).
               05  AE-VAN-CENTRO-COMPRAS           PIC  X(001).
               05  AE-VAN-CODIGO-AMEX              PIC  9(010).
               05  AE-VAN-CNPJ-MATRIZ              PIC  X(015).
               05  AE-VAN-CGC-CPF                  PIC  X(015).
               05  AE-VAN-INSCR-MUNICIPAL          PIC  9(015).
               05  AE-VAN-RAZAO-SOCIAL             PIC  X(032).
               05  FILLER                          PIC  X(022).
               05  AE-VAN-NOME-PLAQ                PIC  X(022).
               05  AE-VAN-MCC-EC                   PIC  X(005).
               05  AE-VAN-LOCALIZACAO              PIC  9(002).
               05  AE-VAN-TIPO-CADEIA              PIC  X(001).
                   88  AE-VAN-PAGTO-INDIVIDUAL                VALUE 'I'.
                   88  AE-VAN-PAGTO-CENTRALIZADO              VALUE 'C'.
               05  AE-VAN-NOME-CONTATO             PIC  X(032).
               05  AE-VAN-EMAIL                    PIC  X(060).
               05  AE-VAN-TIPO-LOGRAD              PIC  X(003).
               05  AE-VAN-ENDERECO                 PIC  X(032).
               05  AE-VAN-NUMERO                   PIC  X(006).
               05  AE-VAN-COMPLEMENTO              PIC  X(020).
               05  AE-VAN-BAIRRO                   PIC  X(020).
               05  AE-VAN-CIDADE                   PIC  X(028).
               05  AE-VAN-ESTADO                   PIC  X(002).
               05  AE-VAN-CEP.
                   07  AE-VAN-CEP5                 PIC  9(005).
                   07  AE-VAN-CEP3                 PIC  9(003).
               05  AE-VAN-CORR-TIPO-LOGRAD         PIC  X(003).
               05  AE-VAN-CORR-ENDERECO            PIC  X(032).
               05  AE-VAN-CORR-NUMERO              PIC  X(006).
               05  AE-VAN-CORR-COMPLEMENTO         PIC  X(020).
               05  AE-VAN-CORR-BAIRRO              PIC  X(020).
               05  AE-VAN-CORR-CIDADE              PIC  X(028).
               05  AE-VAN-CORR-ESTADO              PIC  X(002).
               05  AE-VAN-CORR-CEP.
                   07  AE-VAN-CORR-CEP5            PIC  9(005).
                   07  AE-VAN-CORR-CEP3            PIC  9(003).
               05  FILLER                          PIC  X(020).
               05  AE-VAN-PROPR1                   PIC  X(032).
               05  AE-VAN-PROPR1-CPF               PIC  X(015).
               05  AE-VAN-PROPR1-DATA              PIC  9(006).
               05  AE-VAN-PROPR2                   PIC  X(032).
               05  AE-VAN-PROPR2-CPF               PIC  X(015).
               05  AE-VAN-PROPR2-DATA              PIC  9(006).
               05  AE-VAN-PROPR3                   PIC  X(032).
               05  AE-VAN-PROPR3-CPF               PIC  X(015).
               05  AE-VAN-PROPR3-DATA              PIC  9(006).
               05  AE-VAN-BANCO                    PIC  9(003).
               05  AE-VAN-AGENCIA                  PIC  X(005).
               05  AE-VAN-CONTA                    PIC  X(014).
               05  AE-VAN-DISC                     PIC  9(003)V99.
               05  AE-VAN-CHARGBACK-IMED           PIC  X(001).
               05  AE-VAN-PRAZO                    PIC  9(002).
               05  AE-VAN-IND-ENVIA-DEB-BCO        PIC  X(001).
               05  AE-VAN-TAR-EXTR                 PIC  X(001).
               05  AE-VAN-VL-TAR-EXTR              PIC  9(003)V99.
               05  AE-VAN-PER-TAR-EXTR             PIC  X(001).
               05  AE-VAN-TAR-ONL                  PIC  X(001).
               05  AE-VAN-VL-TAR-ONL               PIC  9(003)V99.
               05  AE-VAN-PER-TAR-ONL              PIC  X(001).
               05  AE-VAN-TAR-EXTR-ELETR           PIC  X(001).
               05  AE-VAN-VL-TAR-EXTR-ELETR        PIC  9(003)V99.
               05  AE-VAN-PER-TAR-EXTR-ELETR       PIC  X(001).
               05  AE-VAN-TAR-AFILIACAO            PIC  X(001).
               05  AE-VAN-VL-TAR-AFIL              PIC  9(003)V99.
               05  AE-VAN-TAXA-CONECTIV-PARCEIRO   PIC  9(005).
               05  AE-VAN-VL-ESTIMAT-FATURAM       PIC  9(012).
               05  AE-VAN-OPER-DOL                 PIC  X(001).
               05  AE-VAN-TIPO-TERMINAL            PIC  X(001).
               05  AE-VAN-VD-MANUAL                PIC  X(001).
               05  AE-VAN-QTDE-MAQUINETA           PIC  9(003).
               05  AE-VAN-QTDE-MEIO-CAPTURA        PIC  9(002).
               05  AE-VAN-DIGITACAO-POS            PIC  X(001).
               05  AE-VAN-PRE-PAG                  PIC  X(001).
               05  AE-VAN-ASSOCIACAO               PIC  9(003).
               05  AE-VAN-VENDA-PARC               PIC  X(001).
               05  AE-VAN-TAXA-PARCELADO           PIC  9(003)V99.
               05  AE-VAN-PRAZO-PARCELADO          PIC  9(003).
               05  AE-VAN-QTDE-PARCELAS            PIC  9(002).
               05  AE-VAN-HOR-FUNC-DIAS-UT         PIC  X(008).
               05  AE-VAN-HOR-FUNC-SABADO          PIC  X(008).
               05  AE-VAN-DT-ASSINAT-CONTR         PIC  X(008).
               05  AE-VAN-DDD                      PIC  9(003).
               05  AE-VAN-FONE                     PIC  9(009).
               05  AE-VAN-DDD-FAX                  PIC  9(003).
               05  AE-VAN-NRO-FAX                  PIC  9(009).
               05  AE-VAN-ASSINATURA-ARQ           PIC  X(001).
               05  AE-VAN-MOB-PAYMENT              PIC  X(001).
               05  AE-VAN-ECOMMERCE                PIC  X(001).
               05  AE-VAN-EC-VAN                   PIC  9(010).
               05  AE-VAN-LIB-DIG-POS              PIC  X(001).
               05  AE-VAN-INSCR                    PIC  X(014).
               05  AE-VAN-NACIONAL-PROPR1          PIC  X(030).
               05  AE-VAN-NRO-DOC-PROPR1           PIC  X(011).
               05  AE-VAN-TIPO-DOC-PROPR1          PIC  X(001).
               05  AE-VAN-NACIONAL-PROPR2          PIC  X(030).
               05  AE-VAN-NRO-DOC-PROPR2           PIC  X(011).
               05  AE-VAN-TIPO-DOC-PROPR2          PIC  X(001).
               05  AE-VAN-NACIONAL-PROPR3          PIC  X(030).
               05  AE-VAN-NRO-DOC-PROPR3           PIC  X(011).
               05  AE-VAN-TIPO-DOC-PROPR3          PIC  X(001).
               05  AE-VAN-NOME-FANT                PIC  X(032).
               05  FILLER                          PIC  X(001).

           03  AE-VAN-T-TRAILER  REDEFINES  AE-VAN-HEADER.
               05  AE-VAN-T-IDENT-LINHA            PIC  9(001).
               05  AE-VAN-T-QTD-REG                PIC  9(008).
               05  FILLER                          PIC  X(991).
