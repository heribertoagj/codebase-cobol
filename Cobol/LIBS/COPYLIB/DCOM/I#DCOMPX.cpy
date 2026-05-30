      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPX                                      *
      * TAMANHO : 0400 BYTES                                           *
      * ARQUIVO : PARCELAS (EMISSAO AVISOS E EXTRATOS)                 *
      ******************************************************************
       01  PX-REGISTRO.
  1 10     03  PX-DMOVTC-DESC-COML          PIC  X(10).
 11  2     03  PX-COD-LANCAMENTO            PIC S9(03)V      COMP-3.
 13  4     03  PX-CINDCD-MOTVO-EVNTO-ORIGE  PIC  X(04).
 17  3     03  PX-DANO-OPER-DESC            PIC S9(04)V      COMP-3.
 20  5     03  PX-NSEQ-OPER-DESC            PIC S9(09)V      COMP-3.
 25  3     03  PX-NPCELA-DESC               PIC S9(05)V      COMP-3.
 28  2     03  PX-CSIT-OPER-DESC            PIC S9(03)V      COMP-3.
 30 10     03  PX-DVCTO-PCELA-DESC          PIC  X(10).
 40  9     03  PX-VPRINC-PCELA-DESC         PIC S9(15)V9(02) COMP-3.
 49  9     03  PX-VIOF-PCELA-DESC           PIC S9(15)V9(02) COMP-3.
 58  9     03  PX-VJURO-PCELA-DESC          PIC S9(15)V9(02) COMP-3.
 67  9     03  PX-VSDO-DVDOR-PRINC          PIC S9(15)V9(02) COMP-3.
 76  9     03  PX-VIOF-PCELA-VENCD          PIC S9(15)V9(02) COMP-3.
 85  9     03  PX-VJURO-MORA-LEGAL          PIC S9(15)V9(02) COMP-3.
 94  9     03  PX-VJURO-MORA-GERC           PIC S9(15)V9(02) COMP-3.
103 10     03  PX-DTRNSF-CRATS-PCELA        PIC  X(10).
113  9     03  PX-VJURO-TRNSF-CRATS-PCELA   PIC S9(15)V9(02) COMP-3.
122  9     03  PX-CTITLO-COBR-BCO           PIC S9(18)V      COMP-3.
131 15     03  PX-SEUNUMERO                 PIC  X(15).
146  5     03  PX-CCNPJ-CPF-PGTO            PIC S9(09)V      COMP-3.
151  3     03  PX-CFLIAL-CNPJ-PGTO          PIC S9(05)V      COMP-3.
154  2     03  PX-CCTRL-CNPJ-PGTO           PIC S9(02)V      COMP-3.
156  5     03  PX-CCNPJ-CPF-SACDO           PIC S9(09)V      COMP-3.
161  3     03  PX-CFLIAL-CNPJ-SACDO         PIC S9(05)V      COMP-3.
164  2     03  PX-CCTRL-CNPJ-SACDO          PIC S9(02)V      COMP-3.
166 60     03  PX-IPSSOA-SACDO              PIC  X(60).
226  1     03  PX-CTPO-BAIXA-PCELA          PIC  X(01).
227 10     03  PX-DEFETV-BAIXA-PCELA        PIC  X(10).
237  9     03  PX-VEFETV-BAIXA-PCELA        PIC S9(15)V9(02) COMP-3.
246  2     03  PX-CBCO-OPER-BAIXA-PCELA     PIC S9(03)V      COMP-3.
248  5     03  PX-CAG-OPER-BAIXA-PCELA      PIC S9(05)V      COMP-3.
253  9     03  PX-VRECTA-BAIXA-EXCED        PIC S9(15)V9(02) COMP-3.
262  5     03  PX-CCNPJ-CPF-DESTINO         PIC S9(09)V      COMP-3.
267  3     03  PX-CFLIAL-CNPJ-DESTINO       PIC S9(05)V      COMP-3.
270  2     03  PX-CCTRL-CNPJ-DESTINO        PIC S9(02)V      COMP-3.
272  2     03  PX-CSPROD-DESC               PIC S9(03)V      COMP-3.
274  1     03  PX-CTPO-CLIENTE              PIC S9(01)V      COMP-3.
275  9     03  PX-VPRINC-BX-PCELA           PIC S9(15)V99    COMP-3.
284  9     03  PX-VJURO-BX-PCELA            PIC S9(15)V99    COMP-3.
293  9     03  PX-VIOF-BX-PCELA             PIC S9(15)V99    COMP-3.
      *------ DADOS COMPLEMENTARES DO SUB-PRODUTO ------
302 10     03  PX-DINIC-PER-AV              PIC  X(10).
312 10     03  PX-DFNAL-PER-AV              PIC  X(10).
322 10     03  PX-NM-REDUZIDO               PIC  X(10).
332  3     03  PX-CJUNC-DEPDC-DEST          PIC S9(05)V      COMP-3.
335  7     03  PX-CCCORR-DEST               PIC S9(13)V      COMP-3.
342  3     03  PX-CGRP-CTBIL-DEST           PIC S9(05)V      COMP-3.
345  2     03  PX-CSGRP-CTBIL-DEST          PIC S9(03)V      COMP-3.
347  9     03  PX-VSDO-DVDOR-PCELA          PIC S9(15)V99    COMP-3.
356  3     03  PX-TPCELA-DESC               PIC S9(05)V      COMP-3.
359 43     03  FILLER                       PIC  X(43).
