      ******************************************************************
      * SISTEMA..: DCOM - I#DCOMT9                                     *
      * TAMANHO..: 700 BYTES                                           *
      * ARQUIVO..: SALDO DIARIA DE NPR                                 *
      * OBJETIVO.: ARQUIVO COM O SALDO DIARIA DE OPERACOES DE NPR PARA *
      *            CALCULO DE SALDO MEDIO MENSAL PARA ENVIO AO BACEN.  *
      ******************************************************************

       01  T9-REG-SALDODIA.
           05  T9-DANO-OPER-DESC          PIC  9(004).
           05  T9-NSEQ-OPER-DESC          PIC  9(009).
           05  T9-NPCELA-DESC-COML        PIC  9(005).
           05  T9-CBCO-CHEQC              PIC  9(003).
           05  T9-CAG-BCRIA-CHEQC         PIC  9(005).
           05  T9-CCCORR-CHEQC            PIC  9(013).
           05  T9-CCHEQ-CHEQC             PIC  9(007).
           05  T9-DACUM-SDO-BACEN         PIC  X(010).
           05  T9-DMES-SDO-PTCAO          PIC  9(002).
           05  T9-QDIA-UTIL               PIC  9(003).
           05  T9-TSDO-MED-UTIL           PIC  9(015)V9(002).
           05  T9-TSDO-MED-VCEDO          PIC  9(015)V9(002).
           05  T9-TSDO-ULT-DIA            PIC  9(015)V9(002).
           05  T9-TSDO-PRINC-ULT          PIC  9(015)V9(002).
           05  T9-TSDO-VCEDO-ULT          PIC  9(015)V9(002).
           05  T9-TSDO-PRINC-VCEDO        PIC  9(015)V9(002).
           05  T9-DVCTO-PCELA-DESC        PIC  X(010).
           05  T9-CSIT-DESC-COML          PIC  9(003).
           05  T9-VPRINC-PCELA-DESC       PIC  9(015)V9(002).
           05  T9-VJURO-PCELA-DESC        PIC  9(015)V9(002).
           05  T9-VJURO-DIA-PCELA         PIC  9(015)V9(002).
           05  T9-VIOF-PCELA-DESC         PIC  9(015)V9(002).
           05  T9-VPSENT-PCELA-DESC       PIC  9(015)V9(002).
           05  T9-VJURO-APROP-ACUM        PIC  9(015)V99.
           05  T9-VSDO-DVDOR-PRINC        PIC  9(015)V9(002).
           05  T9-VIOF-PCELA-VENCD        PIC  9(015)V9(002).
           05  T9-VJURO-MORA-GERC         PIC  9(015)V9(002).
           05  T9-VJURO-MORA-LEGAL        PIC  9(015)V9(002).
           05  T9-CINDCD-PRROG-PCELA      PIC  X(001).
           05  T9-CINDCD-ADTTO-PRZ        PIC  X(001).
           05  T9-CINDCD-ADTTO-TX         PIC  X(001).
           05  T9-CINDCD-ADTTO-VLR        PIC  X(001).
           05  T9-CFUNC-BDSCO             PIC  9(009).
           05  T9-CTERM                   PIC  X(008).
           05  T9-RESERVA-TABELA          PIC  X(150).
           05  T9-FILLER                  PIC  X(001).
           05  T9-CINDCD-CALC-JURO        PIC  X(001).
           05  T9-TXA-JRS-MES             PIC  9(003)V99999.
           05  T9-TXA-JRS-ANO             PIC  9(003)V99999.
           05  T9-CREGLEG                 PIC  9(003).
           05  T9-DINIC-OPER-DESC         PIC  X(010).
           05  T9-ALQ-IOF-VENC            PIC  9(003)V99999.
           05  T9-CREGGERM                PIC  9(003).
           05  T9-CREGGERC                PIC  9(003).
           05  T9-DATA-TRF-CRATS          PIC  X(010).
           05  T9-SITDESC-PROX            PIC  9(003).
           05  T9-PRODUTO                 PIC  9(003).
           05  T9-SUBPROT                 PIC  9(003).
           05  T9-TP-DESCONTO             PIC  9(003).
           05  T9-PESSOA-FJA              PIC  X(001).
           05  T9-DINCL                   PIC  9(008).
           05  T9-RESERVA-ARQUIVO         PIC  X(107).
