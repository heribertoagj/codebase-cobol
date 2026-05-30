      *---------------------------------------------------------------*
      * -INC I#BVVE02                                                 *
      *                                                               *
      *      ARQUIVO ENVIADO PELA VISANET            -  LRECL = 590   *
      *              BOOK NA VISANET:  MMPARRCD                       *
      *                                                               *
      *          SISTEMA PARCELADO LOJISTA - CADASTRO DE PARCELADOS   *
      *                                                               *
      *           UTILIZACAO: CARTEIRA "D" - ANTECIPACAO DE CREDITOS  *
      *                                    ** PARCELADOS **           *
      *                                                               *
      *                         KAMBARA - GP87 - DL06 - DEZEMBRO/1998 *
      *---------------------------------------------------------------*


      *-========     REGISTRO HEADER    =============================-*
       01  VNET-HEADER.
           03  VNET-HEAD-ZEROS1                PIC 9(005)      COMP-3.
           03  VNET-HEAD-ZEROS2                PIC X(001).
           03  VNET-HEAD-ZEROS3                PIC 9(009)      COMP-3.
           03  VNET-HEAD-ZEROS4                PIC X(001).
           03  VNET-HEAD-TIPO-ARQ              PIC 9(002).
           03  VNET-HEAD-NOME-ARQ              PIC X(008).
           03  VNET-HEAD-DT-MOVTO              PIC 9(009)      COMP-3.
           03  VNET-HEAD-DT-PROCESS            PIC 9(009)      COMP-3.
           03  VNET-HEAD-SEQ-REMESSA           PIC 9(005)      COMP-3.
           03  VNET-HEAD-BANCO                 PIC 9(004).
           03  FILLER                          PIC X(548).
           03  VNET-HEAD-SEQ-REGISTRO          PIC 9(009)      COMP-3.


      *-========================--===================================-*
      *-========     REGISTROS DETALHES  ============================-*
       01  VNET-DETALHES.
           03  VNET-DETA-COD-AGE               PIC 9(005)      COMP-3.
           03  VNET-DETA-DIG-AGE               PIC X(001).
           03  VNET-DETA-NRO-CTA               PIC 9(009)      COMP-3.
           03  VNET-DETA-DIG-CTA               PIC X(001).
           03  VNET-DETA-BCO-KEY.
               05  VNET-DETA-PRINCIPAL         PIC 9(003).
               05  VNET-DETA-CHAIN             PIC 9(003).
               05  VNET-DETA-MERCHANT-NRO      PIC 9(010).
               05  VNET-DETA-NR-RO             PIC 9(007).
               05  VNET-DETA-CD-INT            PIC 9(011).
               05  VNET-DETA-ACCT-19           PIC 9(016).
           03  VNET-DETA-REC-TYPE              PIC X(003).
           03  VNET-DETA-BCO-DBA-NAME.
               05  VNET-DETA-NAME-TRUNC        PIC X(017).
               05  VNET-DETA-COD-ADD           PIC X(001).
               05  VNET-DETA-NUM-PARC          PIC 9(002).
               05  VNET-DETA-CD-TRANSACAO.
                   07  VNET-DETA-CD-A          PIC 9(002).
                   07  VNET-DETA-CD-B          PIC 9(001).
               05  VNET-DETA-QT-PARCELAS       PIC 9(002).
           03  VNET-DETA-DT-CAPTURA.
               05  VNET-DETA-AA-DT-CAP         PIC 9(002).
               05  VNET-DETA-MM-DT-CAP         PIC 9(002).
               05  VNET-DETA-DD-DT-CAP         PIC 9(002).
           03  VNET-DETA-MOEDA                 PIC 9(003).
           03  VNET-DETA-DT-SUBMISSAO.
               05  VNET-DETA-AA-DT-SUB         PIC 9(002).
               05  VNET-DETA-MM-DT-SUB         PIC 9(002).
               05  VNET-DETA-DD-DT-SUB         PIC 9(002).
           03  VNET-DETA-QTDE-TOT-PARC         PIC 9(002).
           03  VNET-DETA-QTDE-TOT-PARC-LIB     PIC 9(002).
           03  VNET-DETA-DT-ULT-LIB.
               05  VNET-DETA-AA-DT-ULT-LIB     PIC 9(002).
               05  VNET-DETA-MM-DT-ULT-LIB     PIC 9(002).
               05  VNET-DETA-DD-DT-ULT-LIB     PIC 9(002).
           03  VNET-DETA-DT-LIB.
               05  VNET-DETA-AA-DT-LIB         PIC 9(002).
               05  VNET-DETA-MM-DT-LIB         PIC 9(002).
               05  VNET-DETA-DD-DT-LIB         PIC 9(002).
           03  VNET-DETA-VL-VENDA              PIC S9(015)V99  COMP-3.
           03  VNET-DETA-VL-FINANCIADO         PIC S9(015)V99  COMP-3.
           03  VNET-DETA-VL-ENTRADA            PIC S9(015)V99  COMP-3.
           03  VNET-DETA-VL-TX-EMBARQUE        PIC S9(015)V99  COMP-3.
           03  VNET-DETA-VL-PARCELA            PIC S9(015)V99  COMP-3.
           03  VNET-DETA-VL-RESIDUO            PIC S9(002)V999 COMP-3.
           03  VNET-DETA-SALDO-ABERTO          PIC S9(015)V99  COMP-3.
           03  VNET-DETA-DT-COMPRA.
               05  VNET-DETA-DD-DT-COMP        PIC 9(002).
               05  VNET-DETA-MM-DT-COMP        PIC 9(002).
               05  VNET-DETA-AA-DT-COMP        PIC 9(002).
           03  VNET-DETA-AUTH-CODE             PIC X(006).
           03  VNET-DETA-BCO-PORTADOR          PIC 9(003).
           03  VNET-DETA-DT-MANUT.
               05  VNET-DETA-AA-DT-MANUT       PIC 9(002).
               05  VNET-DETA-MM-DT-MANUT       PIC 9(002).
               05  VNET-DETA-DD-DT-MANUT       PIC 9(002).
           03  VNET-DETA-ACELERACAO.
               05  VNET-DETA-PARC-ACELER       PIC 9(002).
               05  VNET-DETA-DT-ACELER.
                   07  VNET-DETA-AA-DT-ACELE   PIC 9(002).
                   07  VNET-DETA-MM-DT-ACELE   PIC 9(002).
                   07  VNET-DETA-DD-DT-ACELE   PIC 9(002).
               05  VNET-DETA-VR-ACELERADO      PIC S9(015)V99  COMP-3.
           03  VNET-DETA-REEMBOLSO.
               05  VNET-DETA-PARC-REEMB        PIC 9(002).
               05  VNET-DETA-DT-REEMB.
                   07  VNET-DETA-AA-DT-REEMB   PIC 9(002).
                   07  VNET-DETA-MM-DT-REEMB   PIC 9(002).
                   07  VNET-DETA-DD-DT-REEMB   PIC 9(002).
               05  VNET-DETA-VR-REEMBOLSO      PIC S9(015)V99  COMP-3.
           03  FILLER                          PIC X(028).
           03  VNET-SETA-BCO-ANTERIORES     OCCURS   3.
               05  VNET-DETA-DT-ANT-BCO        PIC 9(006)       COMP-3.
               05  VNET-DETA-PLANO-ANT         PIC 9(002).
               05  VNET-DETA-VR-FINAN-ANT      PIC S9(015)V99  COMP-3.
               05  VNET-DETA-VR-TOTAL-ANT      PIC S9(015)V99  COMP-3.
               05  VNET-DETA-VR-ENTR-ANT       PIC S9(015)V99  COMP-3.
               05  VNET-DETA-VR-TX-EMB-ANT     PIC S9(015)V99  COMP-3.
               05  VNET-DETA-VR-PARCE-ANT      PIC S9(015)V99  COMP-3.
               05  VNET-DETA-ACCT-19-ANT       PIC 9(016).
           03  VNET-DETA-BCO-STATUS            PIC X(003).
           03  VNET-DETA-ACELERA-PORTADOR      PIC 9(002).
           03  VNET-DETA-CESSAO-DIREITOS       PIC 9(007)      COMP-3.
           03  FILLER                          PIC X(001).
           03  VNET-DETA-VR-CHARGE-BACK        PIC S9(015)V99  COMP-3.
           03  VNET-DETA-DADOS-ORIGINAL.
               05  VNET-DETA-PLANO-ORIG        PIC 9(002).
               05  VNET-DETA-VR-VENDA-ORIG     PIC S9(007)V99  COMP-3.
               05  VNET-DETA-VR-FINAN-ORIG     PIC S9(007)V99  COMP-3.
               05  VNET-DETA-VR-ENTR-ORIG      PIC S9(007)V99  COMP-3.
               05  VNET-DETA-VR-TX-EMB-ORIG    PIC S9(005)V99  COMP-3.
               05  VNET-DETA-VR-PARCE-ORIG     PIC S9(007)V99  COMP-3.
               05  VNET-DETA-ACCT-19-ORIG      PIC 9(016)      COMP-3.
           03  VNET-DETA-BCO-USERID            PIC X(008).
           03  FILLER                          PIC X(030).
           03  VNET-DETA-FREQUENCIA            PIC 9(002).
           03  VNET-DETA-TAXA                  PIC 9(002)V999.
           03  FILLER                          PIC X(021).
           03  VNET-DETA-SEQ-REGISTRO          PIC 9(009)      COMP-3.

      *-========================--===================================-*
      *-========     REGISTRO TRAILLER  =============================-*
       01  VNET-HEADER.
           03  VNET-TRAL-NOVES1                PIC 9(005)      COMP-3.
           03  VNET-TRAL-NOVES2                PIC X(001).
           03  VNET-TRAL-NOVES3                PIC 9(009)      COMP-3.
           03  VNET-TRAL-NOVES4                PIC X(001).
           03  VNET-TRAL-VLR-TOTAL             PIC 9(015)V99   COMP-3.
           03  FILLER                          PIC X(566).
           03  VNET-TRAL-SEQ-REGISTRO          PIC 9(009)      COMP-3.
