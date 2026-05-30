      ******************************************************************
      *    NOME DO BOOK: SLIGW564                                      *
      *    FUNCAO      : LAYOUT ARQ. ENTRADA DA SLIG- DCON             *
      *    FORMATO     : FIXO                                          *
      *    TAMANHO     : 210                                           *
      ******************************************************************
       01 W564-REG.
          03 W564-HEADER.
             05 W564-TIPO-REGISTRO-H      PIC 9(001).
             05 W564-CCUSTO-ORIGEM-H      PIC 9(003).
             05 W564-HR-SOLICITACAO-H     PIC X(020).
             05 FILLER                    PIC X(186).
          03 W564-DETALHE    REDEFINES W564-HEADER.
             05 W564-TIPO-REGISTRO-D      PIC 9(001).
             05 W564-BCO-CLIENTE          PIC 9(003) COMP-3.
             05 W564-AGENCIA-CLIENTE      PIC 9(005) COMP-3.
             05 W564-CONTA-CLIENTE        PIC 9(013) COMP-3.
             05 W564-CTPO-CART            PIC 9(003) COMP-3.
             05 W564-CCLI-COBR            PIC 9(009) COMP-3.
             05 W564-CGC-CPF-CLIENTE      PIC 9(009) COMP-3.
             05 W564-FILIAL-CLIENTE       PIC 9(005) COMP-3.
             05 W564-CONTROLE-CLIENTE     PIC 9(002).
             05 W564-BCO-SACADO           PIC 9(003) COMP-3.
             05 W564-AGENCIA-SACADO       PIC 9(005) COMP-3.
             05 W564-CONTA-SACADO         PIC 9(013) COMP-3.
             05 W564-CGC-CPF-SACADO       PIC 9(009) COMP-3.
             05 W564-FILIAL-SACADO        PIC 9(005) COMP-3.
             05 W564-CONTROLE-SACADO      PIC 9(002).
             05 W564-VL-PRINC-PROD        PIC 9(016)V99 COMP-3.
             05 W564-VL-PAGO-PROD         PIC 9(015) COMP-3.
             05 W564-VL-DESC-ABAT-PROD    PIC 9(015) COMP-3.
             05 W564-DT-VENCTO-PROD       PIC 9(009) COMP-3.
             05 W564-DT-PAGTO-PROD        PIC 9(009) COMP-3.
             05 W564-DT-ENTRADA-ORIGEM    PIC 9(009) COMP-3.
             05 W564-DT-ENTRADA-SISTEMA   PIC 9(009) COMP-3.
             05 W564-DT-VENCTO-ANTERIOR   PIC 9(009) COMP-3.
             05 W564-CD-PRODUTO           PIC 9(002) COMP-3.
             05 W564-CD-CNEGOC            PIC 9(011) COMP-3.
             05 W564-CD-NOSNRO            PIC 9(011) COMP-3.
             05 W564-CD-SEQUENCIA         PIC 9(003) COMP-3.
             05 W564-CD-COD-NTAX          PIC 9(005) COMP-3.
             05 W564-CD-OCO-REM-L         PIC 9(003) COMP-3.
             05 W564-CD-MOT-REM-SIS       PIC 9(003) COMP-3.
             05 W564-CD-ESPECIE           PIC 9(003) COMP-3.
             05 W564-CD-ORIGEM            PIC 9(003) COMP-3.
             05 W564-DT-INICIO-PER        PIC 9(009) COMP-3.
             05 W564-DT-FINAL-PER         PIC 9(009) COMP-3.
             05 W564-IPSSOA-SACDO         PIC X(060).
             05 FILLER                    PIC X(010).
          03 W564-TRAILLER REDEFINES W564-HEADER.
             05 W564-TIPO-REGISTRO-T      PIC 9(001).
             05 W564-QTD-DETALHES-T       PIC 9(015).
             05 FILLER                    PIC X(194).
      ******************************************************************
      *    FIM DO BOOK I#W564                                          *
      ******************************************************************

