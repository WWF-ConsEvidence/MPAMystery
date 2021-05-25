max_length=255from django.contrib.gis.db import models
from django.contrib.auth import get_user_model
from django.utils.translation import ugettext_lazy as _
from django.core.validators import MinValueValidator, MaxValueValidator
from django.utils import timezone
from django.contrib.gis.db.models.fields import PolygonField, MultiPolygonField
from django.db import connection

NODATA = (995, "Tidak Ada data / No data")
SKIP_CODES = [ # Used across all three instruments (FGD, KII, Household)
    (993,"Pertanyaan tidak diminta (tidak ada skip logic) / Question not asked (no skip logic)"),
    (994, "SKIP / Skipped based on survey skip logic"),
    NODATA,
    (996, "Lainnya / Other"),
    (997, "Tidak tahu / Do not know"),
    (998, "Tidak sesuai / Not applicable"),
    (999, "Menolak /  Refused"),
]
SKIP_CODE_KEYS = [code[0] for code in SKIP_CODES]

DAY_OF_MONTH_CHOICES = list(zip(*[range(1, 31 + 1)] * 2)) + SKIP_CODES # Can probably delete since I delete fields referring to this
MONTH_CHOICES = [ # Can probably delete since I delete fields referring to this
    (1, "January"),
    (2, "February"),
    (3, "March"),
    (4, "April"),
    (5, "May"),
    (6, "June"),
    (7, "July"),
    (8, "August"),
    (9, "September"),
    (10, "October"),
    (11, "November"),
    (12, "December"),
] + SKIP_CODES
YEAR_CHOICES = zip(*[range(2000, 2050 + 1)] * 2)  # [(i, str(i)) for i in range(1,61)] (FGD, KII, and Household)
TREATMENT_CHOICES = [(0, "Control"), (1, "Treatment")] + SKIP_CODES #Used in Treatment model ONLY (refered to by FGD, KII, and Household)
YES_NO_CHOICES = [(0, "Tidak / No"), (1, "Ya / Yes")] + SKIP_CODES #Used across all three instruments (FGD, KII, and Household)

ATT_SCALE_CHOICES = [ #Used in Household model ONLY (part of household survey-related models)
    (1, "Apakah anda sangat tidak setuju / Strongly disagree"),
    (2, "Tidak setuju / Disagree"),
    (3, "Netral / Neither agree nor disagree"),
    (4, "Setuju atau / Agree"),
    (5, "Dangat setuju dengan pernyataan ini / Strongly agree"),
] + SKIP_CODES
ECONOMIC_STATUS_TREND_CHOICES = [ #Used in Household model ONLY (part of household survey-related models)
    (1, "Mejadi sangat buruk / Much worse"),
    (2, "Menjadi sedikit lebih buruk / Slightly worse"),
    (3, "Tidak berubah / No change"),
    (4, "Menjadi sedikit lebih baik / Slightly better"),
    (5, "Menjadi sangat baik / Much better"),
] + SKIP_CODES
EDUCATION_LEVEL_CHOICES = [ # Used in Demographics model ONLY (part of household survey-related models)
    (0, "Tidak Ada Pendidikan Formal / No Formal Education"),
    (1, "Taman Kanak-kanak / Pre-School"),
    (2, "Sekolah Dasar (SD) / Primary School"),
    (3, "Sekolah Menengah Pertama (SMP) / Middle  School "),
    (4,"Sekolah Menengah Atas (SMA) dan Sekolah Menengah Kejuruan (SMK)/ Secondary School"),
    (5, " Ahli Madya Diploma 3 dan lebih tinggi  (S1, S2, S3) / Post Secondary School"),
] + SKIP_CODES
GENDER_CHOICES = [(1, "Laki-Laki / Male"), (2, " Perempuan / Female")] + SKIP_CODES
KII_FREQ_CHOICES = [ #Used in KII AND Zone models only (both are KII-related tables)
    (1, "Tidak pernah / Never"),
    (2, "Hampir tidak pernah / Rarely"),
    (3, "Kadang-kadang / Sometimes"),
    (4, "Biasanya / Usually"),
    (5, "Selalu / Always"),
] + SKIP_CODES
MONITORING_FREQUENCY_CHOICES = [  # Used in User AND Stakeholder model only (both are FGD-related tables)
    (1, "Kurang dari satu kali per tahun / Less than one time per year"),
    (2, "Beberapa kali per tahun / A few times per year"),
    (3, "Beberapa kali per bulan / A few times per month"),
    (4, "Berberapa kali per minggu / A few times per week"),
    (5, "Lebih dari satu kali sehari / More than once per day"),
] + SKIP_CODES
ORGANIZATION_POSITION_CHOICES = [ # Used in NonMarineOrganizationMembership AND MarineOrganizationMembership model ONLY (both are part of household survey-related models)
    (1, "Anggota / Member"),
    (2, " Pengurus / Official"),
] + SKIP_CODES
RELATIONSHIP_CHOICES = [ # Used in Demographics model ONLY (part of household survey-related models)
    (1, "Pasangan (suami/istri) / Spouse"),
    (2, "Anak / Child"),
    (3, "Ibu/Ayah mertua / Father/Mother in law"),
    (4, "Cucu / Grandchild"),
    (5, "Orang tua / Parent"),
    (6, "Anak mantu or Anak menantu  / Child in law"),
    (7, "Saudara laki-laki/perempuan / Sibling"),
    (8, "Ipar / Sibling in law"),
    (9, "Paman/Bibi (Om/Tante) / Uncle or Aunt"),
    (10, "Keponakan / Nephew or Neice"),
    (11, "Anak tiri or Anak angkat / Foster child"),
    (12, "Keluarga lainnya / Other family member"),
    (13, "Tidak ada hubungan kekerabatan / Not related to family"),
] + SKIP_CODES
RELIGION_CHOICES = [ # Used in Household model ONLY (part of household survey-related models)
    (1, "Kristen / Christian"),
    (2, "Islam / Muslim"),
    (3, "Hindu / Hindu"),
    (4, "Budha / Buddhist"),
    (5, "Yahudi / Jewish"),
    (6, "Kepercataan Tradisional / Traditional Beliefs"),
    (7, "Atheis / Atheist"),
] + SKIP_CODES


class MinValueBCValidator(MinValueValidator):
    def compare(self, a, b):
        return a < b and a not in SKIP_CODE_KEYS


class MaxValueBCValidator(MaxValueValidator):
    def compare(self, a, b):
        return a > b and a not in SKIP_CODE_KEYS


class BaseModel(models.Model):
    created_on = models.DateTimeField(auto_now_add=True)
    updated_on = models.DateTimeField(auto_now=True)
    updated_by = models.ForeignKey(
        "auth.User",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="%(class)s_updated_by",
    )

    @classmethod
    def truncate(cls):
        with connection.cursor() as cursor:
            cursor.execute('TRUNCATE TABLE "{0}" CASCADE'.format(cls._meta.db_table))

    class Meta:
        abstract = True


class BaseLookupModel(BaseModel):
    class Meta:
        abstract = True


class BaseChoiceModel(BaseModel):
    @property
    def choice(self):
        ret = {"id": self.pk, "name": self.__str__(), "updated_on": self.updated_on}
        if hasattr(self, "val"):
            ret["val"] = self.val
        return ret

    class Meta:
        abstract = True


class AreaMixin(models.Model):
    def get_polygon(self):
        for f in self._meta.get_fields():
            if isinstance(f, PolygonField) or isinstance(f, MultiPolygonField):
                return getattr(self, f.attname)  # return poly object, not field
        return None

    @property
    def area(self):
        field = self.get_polygon()
        if field is None:
            return None
        if hasattr(self, "_area"):
            return self._area
        # using a world equal area projection to do the areal measurement; there may be a better one
        # https://epsg.io/3410
        # Thought geography=True would make this unnecessary
        self._area = round(field.transform(3410, clone=True).area / 10000, 3)
        return self._area

    area.fget.short_description = _("area (ha)")

    class Meta:
        abstract = True


class Country(BaseChoiceModel):
    # TODO: Why do we need to specify this? And I kept iso as potentially useful.
    # Keep in case data from other countries is added. I agree with keeping iso.
    #Countryid actually comes from the numeric code associated with each iso.
    countryid = models.PositiveIntegerField(primary_key=True)
    iso = models.CharField(max_length=5)
    name = models.CharField(max_length=255)

    class Meta:
        verbose_name_plural = "countries"
        ordering = ("name",)

    def __unicode__(self):
        return _("%s") % self.name

    def __str__(self):
        return self.name


class UserProfile(BaseModel):
    user = models.OneToOneField(
        get_user_model(),
        related_name="profile",
        on_delete=models.CASCADE,
        primary_key=True,
    )
    mpa_interviewyears = models.ManyToManyField(
        "MPAInterviewYear", blank=True, verbose_name="MPA interview years"
    )

    def __str__(self):
        return f"{self.user.username} profile"


class MPA(BaseModel, AreaMixin):
    mpaid = models.IntegerField(primary_key=True)
    country = models.ForeignKey(
        Country, on_delete=models.SET_NULL, null=True, blank=True
    )
    mpaname = models.CharField(max_length=255)
    mpanetwork = models.ForeignKey(
        "MPANetwork", on_delete=models.SET_NULL, null=True, blank=True
    )
    seascape = models.ForeignKey(
        "Seascape", on_delete=models.SET_NULL, null=True, blank=True
    )
    wdpaid = models.IntegerField(null=True, blank=True)
    estyear = models.PositiveSmallIntegerField(
        validators=[MaxValueValidator(timezone.now().year)],
        verbose_name=_("year established"),
        null=True,
        blank=True,
    )
    notes = models.TextField(default=str(NODATA[0]))
    boundary = models.MultiPolygonField(geography=True, null=True, blank=True)
    size = models.IntegerField(verbose_name=_("Size (km2)"), null=True, blank=True)

    class Meta:
        verbose_name = _("MPA")
        verbose_name_plural = _("MPAs")
        ordering = ("name", "est_year")

    def __unicode__(self):
        return _("%s") % self.mpaname

    def __str__(self):
        return self.mpaname


class MPAInterviewYear(BaseModel):
    mpa = models.ForeignKey("MPA", on_delete=models.PROTECT)
    year = models.PositiveSmallIntegerField(
        validators=[MinValueValidator(2000), MaxValueValidator(2050)]
    )

    class Meta:
        verbose_name = "MPA interview year"

    def __str__(self):
        return f"{self.mpa.name} {self.year}"


class FGD(BaseModel):
    fgdid = models.IntegerField(primary_key=True)
    country = models.ForeignKey("Country", on_delete=models.PROTECT)
    settlement = models.ForeignKey("Settlement", on_delete=models.PROTECT)
    fgdcode = models.PositiveSmallIntegerField(
        # TODO: I removed null=True etc. It should be either null or 995, not both.
        # Thanks. I agree.
        default=NODATA[0],
        # TODO: Do we need these validators?
        # No, we don't technically need them. I just did it to avoid any data entry errors. I'm OK with deleting.
        validators=[MinValueBCValidator(1), MaxValueBCValidator(999)],
    )
    facilitator = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="fgd_staff_facilitator",
    )
    notetaker = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        default=NODATA[0],  # 995 is option in this lkp
        related_name="fgd_staff_notetaker",
    )
    fgdate = models.DateField(null=True, blank=True)
    yearmonitoring = models.PositiveSmallIntegerField(
        choices=YEAR_CHOICES,
        validators=[MinValueValidator(2000), MaxValueValidator(2050)],
    )
    starttime = models.TimeField()
    endtime = models.TimeField()
    maleparticipants = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(100)], default=NODATA[0]
    )
    femaleparticipants = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(100)], default=NODATA[0]
    )
    fgdversion = models.ForeignKey(
        "FGDSurveyVersion",
        on_delete=models.PROTECT,
        default=NODATA[0],  # 995 is option inf this lkp tbl
    )
    fgroundname = models.CharField(max_length=255, default=str(NODATA[0]))
    fgroundboat = models.CharField(max_length=255, default=str(NODATA[0]))
    fgroundtime = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    fgrounddist = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    fgroundsize = models.DecimalField(
        max_digits=10,
        decimal_places=6,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    mpaname = models.CharField(
        max_length=255,default=str(NODATA[0])
    )
    mpaboat = models.CharField(max_length=255, default=str(NODATA[0]))
    mpatime = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    mpadist = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    mpasize = models.DecimalField(
        max_digits=10,
        decimal_places=6,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    ntname = models.CharField(max_length=255, default=str(NODATA[0]))  # See above -- never > 100?
    ntboat = models.CharField(max_length=255, default=str(NODATA[0]))
    nttime = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    ntdist = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    ntsize = models.DecimalField(
        max_digits=10,
        decimal_places=6,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    mpahistl = models.TextField(default=str(NODATA[0]))
    mpahist = models.TextField(default=str(NODATA[0]))
    extbnd = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(1), MaxValueBCValidator(100)],
        default=NODATA[0]
    )
    intbnd = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(1), MaxValueBCValidator(100)],
        default=NODATA[0]
    )
    bndlandmarks = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndmarkers = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndsigns = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndgovnotice = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndwoutreach = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndaoutreach = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndvoutreach = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndword = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndotheroutreach = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    bndotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0]))
    bndotherspecify = models.CharField(max_length=255, default=str(NODATA[0]))
    penaltyverbal = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(999)],
        default=NODATA[0]
    )
    penaltywritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyfines = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyprison = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaltyotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0]))
    penaltyotherspecify = models.CharField(max_length=255, default=str(NODATA[0]))
    npenalty = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(999)],
        default=NODATA[0]
    )
    verbalsanction = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    physicalsanction = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    monetarysanction = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    conflictl = models.TextField(default=str(NODATA[0]))
    conflict = models.TextField(default=str(NODATA[0]))
    # TODO: proofread spelling of fieldnames like this one
    # Thanks. Nice catch. Done.
    conflictusertime = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    conflictofficialtime = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    conflictusercost = models.PositiveIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(1000000000)],
        default=NODATA[0]
    )
    conflictofficialcost = models.PositiveIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(1000000000)],
        default=NODATA[0]
    )
    conflictuserdist = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    conflictofficialdist = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        default=NODATA[0],
        validators=[MinValueValidator(0)],
    )
    otherinfol = models.TextField(default=str(NODATA[0]))
    otherinfo = models.TextField(default=str(NODATA[0]))
    otherpeoplel = models.TextField(default=str(NODATA[0]))
    otherpeople = models.TextField(default=str(NODATA[0]))
    othersourcesl = models.TextField(default=str(NODATA[0]))
    othersources = models.TextField(default=str(NODATA[0]))
    traditionalgovernancel = models.TextField(default=str(NODATA[0]))
    traditionalgovernance = models.TextField(default=str(NODATA[0]))
    conflictn = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    congroup = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    conbtwgroups = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    conbtwgroupngov = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    congov = models.PositiveSmallIntegerField(choices=YES_NO_CHOICES, default=NODATA[0])
    contypemarine = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    contypegov = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    contypeusers = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    contyperec = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    contypeother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    contypeotherspecify = models.TextField(max_length=255, default=str(NODATA[0])
    dataentryid = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="fgd_staff_data_entry",
    )
    datacheckid = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="fgd_staff_data_check",
    )
    notesl = models.TextField(default=str(NODATA[0]))
    notes = models.TextField(default=str(NODATA[0]))
    qaqcnotes = models.TextField(default=str(NODATA[0]))

    @property
    def mpa(self):
        return self.settlement.mpa.mpaid

    class Meta:
        verbose_name = _("FGD")
        verbose_name_plural = _("FGDs")

    def __str__(self):
        return str(self.pk)


class Users(BaseModel):
    userid = models.IntegerField(primary_key=True)
    fgd = models.ForeignKey(FGD, on_delete=models.PROTECT)
    usercode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    usernamel = models.CharField(max_length=255, default=str(NODATA[0]))
    username = models.CharField(max_length=255, default=str(NODATA[0]))
    userextbnd = models.ForeignKey(
        "LkpNoneToAllScale",
        related_name="nonetoall_externalboundary",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    userintbnd = models.ForeignKey(
        "LkpNoneToAllScale",
        related_name="nonetoall_internalboundary",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    participateestablish = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participateboundaries = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participateadmin = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participaterules = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    monitoreco = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    monitorsoc = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    monitorcompliance = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    enforcefreq = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    contributionrank = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(1), MaxValueBCValidator(50)], default=NODATA[0]
    )
    benefitrank = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(1), MaxValueBCValidator(50)], default=NODATA[0]
    )
    monitorcovidl = models.TextField(default=str(NODATA[0]))
    monitorcovid = models.TextField(default=str(NODATA[0]))
    covidassistancel = models.TextField(default=str(NODATA[0]))
    covidassistance = models.TextField(default=str(NODATA[0]))
    conservationimpactcovidl = models.TextField(default=str(NODATA[0]))
    conservationimpactcovid = models.TextField(default=str(NODATA[0]))

    def __str__(self):
        return self.username


class Habitat(BaseModel):
    habitatid = models.IntegerField(primary_key=True)
    fgd = models.ForeignKey(FGD, on_delete=models.PROTECT)
    habitatcode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    habitattypel = models.CharField(max_length=255, default=str(NODATA[0]))
    habitattype = models.CharField(max_length=255,default=str(NODATA[0]))

    def __str__(self):
        return self.habitatcode


class Rule(BaseModel):
    ruleid = models.IntegerField(primary_key=True)
    fgd = models.ForeignKey(FGD, on_delete=models.SET_NULL, null=True, blank=True)
    rulecode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    rulel = models.TextField(default=str(NODATA[0]))
    rule = models.TextField(default=str(NODATA[0]))

    def __str__(self):
        return self.rulecode


class Species(BaseModel):
    speciesid = models.IntegerField(primary_key=True)
    fgdid = models.ForeignKey(FGD, on_delete=models.PROTECT, null=True, blank=True)
    speciescommonl = models.CharField(max_length=255, default=str(NODATA[0]))
    speciescommon = models.CharField(max_length=255, default=str(NODATA[0]))
    family = models.CharField(max_length=255, default=str(NODATA[0]))
    genus = models.CharField(max_length=255, default=str(NODATA[0]))
    # TODO: Is species needed anymore?
    # Yes, the previous fields are for species common names.
    species = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.species


class Stakeholder(BaseModel):
    stakeholderid = models.IntegerField(primary_key=True)
    fgdid = models.ForeignKey(FGD, on_delete=models.PROTECT, null=True, blank=True)
    stakeholdernamel = models.CharField(max_length=255, default=str(NODATA[0]))
    stakeholdername = models.CharField(max_length=255, default=str(NODATA[0]))
    participateestablish = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participateboundaries = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participateadmin = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    participaterules = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    monitoreco = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    monitorsoc = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    monitorcompliance = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )
    enforcefreq = models.PositiveSmallIntegerField(
        choices=MONITORING_FREQUENCY_CHOICES, default=NODATA[0]
    )

    def __str__(self):
        return self.stakeholderid


class LkpMonitoringStaff(BaseModel):
    staffid = models.IntegerField(primary_key=True)  # TODO: remove or rename? # just renamed
    name = models.CharField(max_length=255, blank=True)

    def __str__(self):
        return self.name


class Settlement(BaseModel):
    settlementid = models.IntegerField(primary_key=True)
    mpa = models.ForeignKey(MPA, on_delete=models.PROTECT)
    name = models.CharField(max_length=255)
    treatment = models.PositiveSmallIntegerField(
        choices=TREATMENT_CHOICES, default=NODATA[0]
    )
    districtname = models.CharField(max_length=255)
    districtcode = models.BigIntegerField(blank=True, default=NODATA[0])
    marketname1 = models.CharField(max_length=255, default=str(NODATA[0]))
    marketname2 = models.CharField(max_length=255, default=str(NODATA[0]))
    zone = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.name


class Birth(BaseModel):
    birthid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    nameinfant = models.CharField(max_length=255, default=str(NODATA[0]))
    infantsurvived = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    dateofdeath = models.DateField(null=True, blank=True)

    def __str__(self):
        return str(self.pk)


class Death(BaseModel):
    deathid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    namedeceased = models.CharField(max_length=255, default=str(NODATA[0]))
    gender = models.IntegerField(choices=GENDER_CHOICES, default=NODATA[0])
    ageatdeath = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=NODATA[0],
        validators=[MinValueBCValidator(0), MaxValueBCValidator(150)],
    )
    datedeath = models.DateField(null=True, blank=True)

    def __str__(self):
        return self.pk


class Demographic(BaseModel):
    demographicid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    demographiccode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    individualname = models.CharField(max_length=255, default=str(NODATA[0]))
    relationhhh = models.IntegerField(choices=RELATIONSHIP_CHOICES, default=NODATA[0])
    individualage = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=NODATA[0],
        validators=[MinValueBCValidator(0), MaxValueBCValidator(150)],
    )
    individualgender = models.IntegerField(choices=GENDER_CHOICES, default=NODATA[0])
    individualeducation = models.CharField(max_length=255, default=str(NODATA[0]))
    individualedlevel = models.IntegerField(
        choices=EDUCATION_LEVEL_CHOICES, default=NODATA[0]
    )
    individualenrolled = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    householdhead = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    individualunwell = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    individualdaysunwell = models.PositiveIntegerField(
        validators=[MaxValueBCValidator(31)], default=NODATA[0]
    )
    individuallostdays = models.PositiveIntegerField(
        validators=[MaxValueBCValidator(31)], default=NODATA[0]
    )

    def __str__(self):
        return str(self.demographiccode)


class GlobalStep(BaseModel):
    globalstepsid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    globalmarinesteps = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.globalmarinesteps


class GlobalThreat(BaseModel):
    globalthreatid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    globalmarinethreat = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.globalmarinethreat


class LocalStep(BaseModel):
    localstepsid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    localsteps = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.localsteps


class LocalThreat(BaseModel):
    localthreatid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    localmarinethreat = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.localmarinethreat


class NonMarineOrganizationMembership(BaseModel):
    nmorganizationid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    name = models.CharField(max_length=255, default=str(NODATA[0]))
    position = models.IntegerField(
        choices=ORGANIZATION_POSITION_CHOICES, default=NODATA[0]
    )
    meeting = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    days = models.IntegerField(default=NODATA[0], validators=[MinValueBCValidator(0), MaxValueBCValidator(365)]),
    contribution = (models.IntegerField(default=NODATA[0])
    contributionunits = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.pk


class MarineOrganizationMembership(BaseModel):
    morganizationid = models.IntegerField(primary_key=True)
    household = models.ForeignKey("Household", on_delete=models.PROTECT)
    entryhouseholdid = models.BigIntegerField(default=NODATA[0])
    name = models.CharField(max_length=255, default=str(NODATA[0]))
    position = models.IntegerField(
        choices=ORGANIZATION_POSITION_CHOICES, default=NODATA[0]
    )
    meeting = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    days = models.IntegerField(default=NODATA[0], validators=[MinValueBCValidator(0), MaxValueBCValidator(365)]),
    contribution = (models.IntegerField(default=NODATA[0]),)
    contributionunits = models.CharField(max_length=255, default=str(NODATA[0]))

    def __str__(self):
        return self.pk


class Household(BaseModel):
    COOKING_FUEL_CHOICES = [ # Only used in Household model (part of household survey-related instruments)
        (1, "Listrik/Gas / Electricity or gas"),
        (2, "Minyak/Minyak Tanah / Oil"),
        (3, "Kayu / Wood"),
        (4, "Arang / Charcoal"),
        (5, "Kayu ranting atau serpihan kayu / Small sticks/scrap wood"),
        (6, " Serasah, daun, biogas / Weeds, leaves, dung"),
    ] + SKIP_CODES
    FS_CHOICES = [ # Only used in Household model (part of household survey-related instruments)
        (1, "Sering / Often true"),
        (2, "Kadang-kadang / Sometimes true"),
        (3, "Tidak pernah / Never true"),
    ] + SKIP_CODES
    FS_FREQ_CHOICES = [ # Only used in Household model (part of household survey-related instruments)
        (1, "Hampir setiap bulan / Almost every month"),
        (2,"Beberapa bulan tetapi tidak setiap bulan / Some months but not every month"),
        (3, "Hanya satu atau dua bulan / Only one or two months a year"),
    ] + SKIP_CODES
    SOCIAL_CONFLICT_CHOICES = [ # Only used in Household model (part of household survey-related instruments)
        (1, "Sangat meningkat / Greatly Increased"),
        (2, "Meningkat / Increased"),
        (3, "Tidak ada perubahan / Neither increased nor decreased"),
        (4, "Menurun / Decreased"),
        (5, "Sangat menurum / Greatly decreased"),
    ] + SKIP_CODES


    # TODO: deal with allowing skip codes, or remove after converting to geographic data types
    # For now let's deal with allow skip codes. Can we use a similar workaround with ""...BCValidator" here to allow skip codes?
    LATITUDE_DEGREE_CHOICES = zip(*[range(0, 90 + 1)] * 2)
    LONGITUDE_DEGREE_CHOICES = zip(*[range(0, 180 + 1)] * 2)
    MINUTES_SECONDS_CHOICES = zip(*[range(0, 60 + 1)] * 2)
    SECONDS_FRACTION_CHOICES = zip(*[range(0, 9 + 1)] * 2)
    LATITUDE_SPHERE_CHOICES = [("N", "North"), ("S", "South")] + [
        (str(key), val) for (key, val) in SKIP_CODES
    ]
    LONGITUDE_SPHERE_CHOICES = [("E", "East"), ("W", "West")] + [
        (str(key), val) for (key, val) in SKIP_CODES
    ]

    householdid = models.IntegerField(primary_key=True)
    settlement = models.ForeignKey("Settlement", on_delete=models.PROTECT)
    kkcode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    respondent = models.CharField(max_length=255)
    secondaryrespondent = models.CharField(max_length=255, default=str(NODATA[0]))
    primaryinterviewer = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="household_primaryinterviewer",
    )
    secondaryinterviewer = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="household_secondaryinterviewer",
        default=NODATA[0],  # 995 is option in this lkp table
    )
    fieldcoordinator = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="household_fieldcoordinator",
    )
    # TODO: convert to PointField
    latdeg = models.PositiveSmallIntegerField(
        choices=LATITUDE_DEGREE_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(90)],
        default=NODATA[0],
    )
    latmin = models.PositiveSmallIntegerField(
        choices=MINUTES_SECONDS_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(60)],
        default=NODATA[0],
    )
    latsec = models.PositiveSmallIntegerField(
        choices=MINUTES_SECONDS_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(60)],
        default=NODATA[0],
    )
    latfrac = models.PositiveSmallIntegerField(
        choices=SECONDS_FRACTION_CHOICES,
        default=NODATA[0],
        validators=[MinValueBCValidator(0), MaxValueBCValidator(9)],
    )
    latsphere = models.CharField(
        choices=LATITUDE_SPHERE_CHOICES, max_length=3, default=str(NODATA[0])
    )
    londeg = models.PositiveSmallIntegerField(
        choices=LONGITUDE_DEGREE_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(180)],
        default=NODATA[0],
    )
    lonmin = models.PositiveSmallIntegerField(
        choices=MINUTES_SECONDS_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(60)],
        default=NODATA[0],
    )
    lonsec = models.PositiveSmallIntegerField(
        choices=MINUTES_SECONDS_CHOICES,
        validators=[MinValueBCValidator(0), MaxValueBCValidator(60)],
        default=NODATA[0],
    )
    lonfrac = models.PositiveSmallIntegerField(
        choices=SECONDS_FRACTION_CHOICES,
        default=NODATA[0],
        validators=[MinValueBCValidator(0), MaxValueBCValidator(9)],
    )
    lonsphere = models.CharField(
        choices=LONGITUDE_SPHERE_CHOICES, max_length=3, default=str(NODATA[0]
    )
    yearmonitoring = models.PositiveSmallIntegerField(
        choices=YEAR_CHOICES,
        validators=[MinValueValidator(2000), MaxValueValidator(2050)],
    )
    interviewdate = models.DateField(blank=True, null=True)
    interviewstart = models.TimeField(blank=True, null=True)
    interviewend = models.TimeField(blank=True, null=True)
    interviewlength = models.TimeField(blank=True, null=True)
    surveyversionnumber = models.ForeignKey(
        "HouseholdSurveyVersion", on_delete=models.PROTECT
    )
    usualfish = models.CharField(max_length=255, default=str(NODATA[0]))
    householdsize = models.PositiveSmallIntegerField(default=NODATA[0])
    yearsresident = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(1), MaxValueBCValidator(150)], default=NODATA[0]
    )
    timemarket = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        validators=[MinValueValidator(0)],
        default=NODATA[0],
    )
    primarymarketname = models.CharField(max_length=255, default=str(NODATA[0]))
    secondarymarketname = models.CharField(max_length=255, default=str(NODATA[0]))
    timesecondarymarket = models.DecimalField(
        max_digits=6,
        decimal_places=3,
        validators=[MinValueValidator(0)],
        default=NODATA[0],
    )
    paternalethnicity = models.CharField(max_length=255, default=str(NODATA[0]))
    maternalethnicity = models.CharField(max_length=255, default=str(NODATA[0]))
    religion = models.PositiveSmallIntegerField(
        choices=RELIGION_CHOICES, default=NODATA[0]
    )
    primarylivelihood = models.ForeignKey(
        "LkpLivelihood",
        related_name="livelihood_primarylivelihood",
        on_delete=models.PROTECT,
        default=NODATA[0],  # 995 is an id under this lkp table
    )
    primarylivelihoodyear = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    secondarylivelihood = models.ForeignKey(
        "LkpLivelihood",
        related_name="livelihood_secondarylivelihood",
        on_delete=models.PROTECT,
        default=NODATA[0],  # 995 is an id under this lkp table
    )
    tertiarylivelihood = models.ForeignKey(
        "LkpLivelihood",
        related_name="livelihood_tertiarylivelihood",
        on_delete=models.PROTECT,
        default=NODATA[0],  # 995 is an id under this lkp table
    )
    # TODO: just checking, based on name: this is really yes/no?
    # Yes. The question is "If you had this livelihood for more than one year?"
    tertiarylivelihoodyear = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )

    freqfishtime = models.ForeignKey(
        "LkpFreqFishTime",
        related_name="freqfishtime_freqfish",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    freqsalefish = models.ForeignKey(
        "LkpFreqFishTime",
        related_name="freqfishtime_freqsalefish",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    percentincomefish = models.ForeignKey(
        "LkpNoneToAllScale",
        related_name="nonetoall_percentincomefish",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    freqeatfish = models.ForeignKey(
        "LkpFreqFishTime",
        related_name="freqfishtime_freqeatfish",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    percentproteinfish = models.ForeignKey(
        "LkpNoneToAllScale",
        related_name="nonetoall_percentproteinfish",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    majorfishtechnique = models.ForeignKey(
        "LkpFishTechCategory",
        on_delete=models.PROTECT,
        default=NODATA[0],
    )
    primaryfishtechnique = models.ForeignKey(
        "LkpFishTechnique",
        on_delete=models.PROTECT,
        default=NODATA[0],
        related_name="primaryfishtechnique_households",
    )
    secondaryfishtechnique = models.ForeignKey(
        "LkpFishTechnique",
        on_delete=models.PROTECT,
        default=NODATA[0],
        related_name="secondaryfishtechnique_households",
    )
    tertiaryfishtechnique = models.ForeignKey(
        "LkpFishTechnique",
        on_delete=models.PROTECT,
        default=NODATA[0],
        related_name="tertiaryfishtechnique_households",
    )
    lessproductivedaysfishing = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(365)], default=NODATA[0]
    )
    poorcatch = models.PositiveIntegerField(default=NODATA[0])
    poorcatchunits = models.CharField(max_length=255, default=str(NODATA[0]))
    poorfishincome = models.PositiveIntegerField(default=NODATA[0])
    poorfishincomeunits = models.CharField(max_length=255, default=str(NODATA[0]))
    moreproductivedaysfishing = models.PositiveSmallIntegerField(
        validators=[MinValueBCValidator(0), MaxValueBCValidator(365)], default=NODATA[0]
    )
    goodcatch = models.PositiveIntegerField(default=NODATA[0])
    goodcatchunits = models.CharField(max_length=255, default=str(NODATA[0]))
    goodfishincome = models.PositiveIntegerField(default=NODATA[0])
    goodfishincomeunits = models.CharField(max_length=255, default=str(NODATA[0]))
    economicstatustrend = models.PositiveSmallIntegerField(
        choices=ECONOMIC_STATUS_TREND_CHOICES, default=NODATA[0]
    )
    economicstatusreasonl = models.TextField(default=str(NODATA[0]))
    economicstatusreason = models.TextField(default=str(NODATA[0]))
    economicadjustreasonl = models.TextField(default=str(NODATA[0]))
    economicadjustreason = models.TextField(default=str(NODATA[0]))
    assetcar = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(30)],
        default=NODATA[0]
    )
    assetcarobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcaryear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetcarsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcarsourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assettruck = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assettruckobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assettruckyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assettrucksource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assettrucksourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetcartruck = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetbicycle = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetbicycleobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetbicycleyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetbicyclesource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetbicyclesourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetmotorcycleamount = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetmotorcycleobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetmotorcycleyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetmotorcyclesource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetmotorcyclesourceother = models.CharField(
        max_length=255, default=str(NODATA[0])
    )
    assetboatnomotor = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetboatnomotorobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatnomotoryear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetboatnomotorsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatnomotorsourceother = models.CharField(
        max_length=255, default=str(NODATA[0])
    )
    assetboatoutboard = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetboatoutboardobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatoutboardyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetboatoutboardsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatoutboardsourceother = models.CharField(
        max_length=255, default=str(NODATA[0])
    )
    assetboatinboard = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetboatinboardobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatinboardyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetboatinboardsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetboatinboardsourceother = models.CharField(
        max_length=255, default=str(NODATA[0])
    )
    assetlandlinephone = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetlandlinephoneobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetlandlinephoneyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetlandlinephonesource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetlandlinephonesourceother = models.CharField(
        max_length=255, default=str(NODATA[0])
    )
    assetcellphone = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetcellphoneobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcellphoneyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetcellphonesource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcellphonesourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetphonecombined = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assettv = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assettvobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assettvyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assettvsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assettvsourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetradio = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetradioobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetradioyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetradiosource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetradiosourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetstereo = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetstereoobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetstereoyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetstereosource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetstereosourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetcd = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetcdobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcdyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetcdsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetcdsourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetdvd = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetdvdobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetdvdyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetdvdsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetdvdsourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetentertain = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetsatellite = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetsatelliteobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetsatelliteyear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetsatellitesource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetsatellitesourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    assetgenerator = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(50)], default=NODATA[0]
    )
    assetgeneratorobtain = models.ForeignKey(
            "LkpAssetObtain",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetgeneratoryear = models.PositiveSmallIntegerField(
        validators=[
            MinValueBCValidator(1900),
            MaxValueBCValidator(timezone.now().year),
        ],
        default=NODATA[0],
    )
    assetgeneratorsource = models.ForeignKey(
            "LkpAssetAssistance",
            on_delete=models.SET_NULL,
            null=True,
            blank=True
    )
    assetgeneratorsourceother = models.CharField(max_length=255, default=str(NODATA[0]))
    cookingfuel = models.PositiveSmallIntegerField(
        choices=COOKING_FUEL_CHOICES, default=NODATA[0]
    )
    householddeath = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    householdbirth = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsnotenough = models.PositiveSmallIntegerField(
        choices=FS_CHOICES, default=NODATA[0]
    )
    fsdidnotlast = models.PositiveSmallIntegerField(
        choices=FS_CHOICES, default=NODATA[0]
    )

    fsbalanceddiet = models.PositiveSmallIntegerField(
        choices=FS_CHOICES, default=NODATA[0]
    )

    fsadultskip = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsfreqadultskip = models.PositiveSmallIntegerField(
        choices=FS_FREQ_CHOICES, default=NODATA[0]
    )
    fseatless = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fshungry = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fschildportion = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fslowcostfood = models.PositiveSmallIntegerField(
        choices=FS_CHOICES, default=NODATA[0]
    )
    fschildskip = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsfreqchildskip = models.PositiveSmallIntegerField(
        choices=FS_FREQ_CHOICES, default=NODATA[0]
    )
    fsnomealchild = models.PositiveSmallIntegerField(
        choices=FS_CHOICES, default=NODATA[0]
    )
    rightsaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    rightsharvest = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    rightsmanage = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    rightsexclude = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    rightstransfer = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socialconflict = models.PositiveSmallIntegerField(
        choices=SOCIAL_CONFLICT_CHOICES, default=NODATA[0]
    )
    marinegroup = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    numbermarinegroup = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    othergroup = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    numberothergroup = models.PositiveSmallIntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    votedistrict = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    votenational = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    numlocalthreat = models.IntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    numglobalthreat = models.IntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    numlocalaction = models.IntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    numglobalaction = models.IntegerField(
        validators=[MaxValueBCValidator(25)], default=NODATA[0]
    )
    placehappy = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    placefavourite = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    placemiss = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    placebest = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    placefishhere = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    placebemyself = models.PositiveSmallIntegerField(
        choices=ATT_SCALE_CHOICES, default=NODATA[0]
    )
    primarylivelihoodcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    secondarylivelihoodcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    tertiarylivelihoodcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    freqfishtimecovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    freqsalefishcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    percentincomefishcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    freqeatfishcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    percentproteinfishcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    majorfishtechniquecovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    poorfishincomecovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    goodfishincomecovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsnotenoughcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsdidnotlastcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsbalanceddietcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fseatlesscovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fshungrycovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fschildportioncovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fslowcostfoodcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsfreqchildskipcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    fsnomealchildcovid = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    marinegroupcovid = models.TextField(default=str(NODATA[0]))
    othergroupcovid = models.TextField(default=str(NODATA[0]))
    worstdaycatch = models.CharField(max_length=255, default=str(NODATA[0]))
    worstdaycatchunits = models.CharField(max_length=255, default=str(NODATA[0]))
    bestdaycatch = models.CharField(max_length=255, default=str(NODATA[0]))
    bestdaycatchunits = models.CharField(max_length=255, default=str(NODATA[0]))
    averageincome = models.CharField(max_length=255, default=str(NODATA[0]))
    averageincomeunits = models.CharField(max_length=255, default=str(NODATA[0]))
    worstincome = models.CharField(max_length=255, default=str(NODATA[0]))
    worstincomeunits = models.CharField(max_length=255, default=str(NODATA[0]))
    bestincome = models.CharField(max_length=255, default=str(NODATA[0]))
    bestincomeunits = models.CharField(max_length=255, default=str(NODATA[0]))
    entrycomputeridentifier = models.CharField(max_length=255, default=str(NODATA[0]))
    entryhouseholdid = models.IntegerField(blank=True, null=True)
    pilotreferencecode = models.CharField(max_length=255, default=str(NODATA[0]))
    baseline_t2_pairs = models.FloatField(blank=True, null=True)

    anyotherinfo = models.TextField(default=str(NODATA[0]))
    willingparticipant = models.TextField(default=str(NODATA[0]))
    notes = models.TextField(default=str(NODATA[0]))
    dataentrycomplete = models.BooleanField(blank=True, null=True)
    datacheckcomplete = models.BooleanField(blank=True, null=True)
    dataentryid = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        blank=True,
        null=True,
        related_name="household_dataentrystaff",
    )
    datacheckid = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        blank=True,
        null=True,
        related_name="household_datacheckstaff",
    )

    @property
    def mpa(self):
        return self.settlement.mpa.mpaid

    def __str__(self):
        return str(self.householdid) or ""


class KII(BaseModel):
    kiiid = models.IntegerField(primary_key=True)
    country = models.ForeignKey("Country", on_delete=models.PROTECT)
    settlement = models.ForeignKey("Settlement", on_delete=models.PROTECT)
    kiicode = models.PositiveSmallIntegerField(
        default=NODATA[0], validators=[MinValueBCValidator(1), MaxValueBCValidator(999)]
    )
    fgd = models.ForeignKey("FGD", on_delete=models.PROTECT)
    informantname = models.CharField(max_length=255, default=str(NODATA[0]))
    keyinformantrole = models.CharField(max_length=255, default=str(NODATA[0]))
    primaryinterviewer = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="kii_primaryinterviewer",
    )
    secondaryinterviewer = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.PROTECT,
        related_name="kii_secondaryinterviewer",
        default=NODATA[0],  # 995 is option in this lkp table
    )
    kiidate = models.DateField(blank=True, null=True)
    yearmonitoring = models.PositiveSmallIntegerField(
        choices=YEAR_CHOICES,
        validators=[
            MinValueBCValidator(2000),
            MaxValueBCValidator(2050),
        ],
        blank=True,
        null=True,
    )
    starttime = models.TimeField(blank=True, null=True)
    endtime = models.TimeField(blank=True, null=True)
    kiiversion = models.ForeignKey("KIISurveyVersion", on_delete=models.PROTECT)
    mpahistoryl = models.TextField(default=str(NODATA[0]))
    mpahistory = models.TextField(default=str(NODATA[0]))
    # TODO: This field name is plural; should it be? If you want more than one number we can talk about ways to do that.
    # Keep as plural. No need for more than one number here.
    pilotnzones = models.PositiveSmallIntegerField(default=NODATA[0])
    ecozone = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    soczone = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    druleeco = models.PositiveSmallIntegerField(
        choices=KII_FREQ_CHOICES, default=NODATA[0]
    )
    drulesoc = models.PositiveSmallIntegerField(
        choices=KII_FREQ_CHOICES, default=NODATA[0]
    )
    pilotnestedness = models.PositiveSmallIntegerField(default=NODATA[0])
    rulecomml = models.TextField(default=str(NODATA[0]))
    rulecomm = models.TextField(default=str(NODATA[0]))
    ruleawarel = models.TextField(default=str(NODATA[0]))
    ruleaware = models.TextField(default=str(NODATA[0]))
    rulepracticel = models.TextField(default=str(NODATA[0]))
    rulepractice = models.TextField(default=str(NODATA[0]))
    informalrulel = models.TextField(default=str(NODATA[0]))
    informalrule = models.TextField(default=str(NODATA[0]))
    ruleparticipationl = models.TextField(default=str(NODATA[0]))
    ruleparticipation = models.TextField(default=str(NODATA[0]))
    monitorl = models.TextField(default=str(NODATA[0]))
    monitor = models.TextField(default=str(NODATA[0]))
    penverbal = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penwritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penfines = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penincarceraton = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    penotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    penfreq = models.PositiveSmallIntegerField(
        choices=KII_FREQ_CHOICES, default=NODATA[0]
    )
    penprevious = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    peneco = models.PositiveSmallIntegerField(choices=YES_NO_CHOICES, default=NODATA[0])
    penecon = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    pensoc = models.PositiveSmallIntegerField(choices=YES_NO_CHOICES, default=NODATA[0])
    penwealth = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penpower = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penstatus = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penfactorother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penfactorotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    penfactorotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    incened = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenskills = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenpurchase = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenloan = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenpayment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenemploy = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    incenotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    incenotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    ecomonverbal = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonwritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonposition = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonfine = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonincarceration = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    ecomonotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    ecomonotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    socmonverbal = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonwritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonposition = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonfine = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonincarceration = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    socmonotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    socmonotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    compmonverbal = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonwritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonposition = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonfine = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonincarceration = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    compmonotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    compmonotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    penmonverbal = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonwritten = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonaccess = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonposition = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonequipment = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonfine = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonincarceration = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonother = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    penmonotherspecifyl = models.CharField(max_length=255, default=str(NODATA[0])
    penmonotherspecify = models.CharField(max_length=255, default=str(NODATA[0])
    conflictresl = models.TextField(default=str(NODATA[0]))
    conflictres = models.TextField(default=str(NODATA[0]))
    ecoimpactl = models.TextField(default=str(NODATA[0]))
    ecoimpact = models.TextField(default=str(NODATA[0]))
    socimpactl = models.TextField(default=str(NODATA[0]))
    socimpact = models.TextField(default=str(NODATA[0]))
    contributionl = models.TextField(default=str(NODATA[0]))
    contribution = models.TextField(default=str(NODATA[0]))
    benefitl = models.TextField(default=str(NODATA[0]))
    benefit = models.TextField(default=str(NODATA[0]))
    ecoimpactcovidl = models.TextField(default=str(NODATA[0]))
    ecoimpactcovid = models.TextField(default=str(NODATA[0]))
    socimpactcovidl = models.TextField(default=str(NODATA[0]))
    socimpactcovid = models.TextField(default=str(NODATA[0]))
    mpaimpactcovidl = models.TextField(default=str(NODATA[0]))
    mpaimpactcovid = models.TextField(default=str(NODATA[0]))
    anyotherinfol = models.TextField(default=str(NODATA[0]))
    anyotherinfo = models.TextField(default=str(NODATA[0]))
    anyotherkil = models.TextField(default=str(NODATA[0]))
    anyotherki = models.TextField(default=str(NODATA[0]))
    anyotherdocsl = models.TextField(default=str(NODATA[0]))
    anyotherdocs = models.TextField(default=str(NODATA[0]))
    notesl = models.TextField(default=str(NODATA[0]))
    notes = models.TextField(default=str(NODATA[0]))
    dataentryid = models.ForeignKey(  # See naming suggestion above; if you still want `dataentry` then I suggest
        # renaming the other one (`dataentryid`) to match
        # I agree. See my changes in another similar section to match
        # TODO: checking if you wanted to rename these to match what you changed above?
        # Yes. Just renamed all to "dataentryid" and "datacheckid"
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="kii_staff_data_entry",
    )
    datacheck = models.ForeignKey(
        "LkpMonitoringStaff",
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name="kii_staff_data_check",
    )

    @property
    def mpa(self):
        return self.settlement.mpa.mpaid

    class Meta:
        verbose_name = _("KII")
        verbose_name_plural = _("KIIs")

    def __str__(self):
        return str(self.pk)


class HabitatRule(BaseModel):
    habrulesid = models.IntegerField(primary_key=True)
    kii = models.ForeignKey(KII, on_delete=models.PROTECT)
    habnamel = models.CharField(max_length=255, default=str(NODATA[0]))
    habname = models.CharField(max_length=255, default=str(NODATA[0]))
    habrule = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    habspecificrulel = models.TextField(default=str(NODATA[0]))
    habspecificrule = models.TextField(default=str(NODATA[0]))

    def __str__(self):
        return self.habname


class Right(BaseModel):
    KII_GOVT_SUPPORT_CHOICES = [ #Only used in KII associated tables
        (1, "Sangat menentang / Strongly oppose"),
        (2, "Menentang / Oppose"),
        (3, "Tidak menantang maupan mendukung / Neither oppose nor support"),
        (4, "Mendukung / Support"),
        (5, "Sangat mendukung / Strongly support"),
    ] + SKIP_CODES
    KII_RULE_INCLUDED_CHOICES = [ #Only used in KII associated tables
        (1, "Tidak dimasukkan / Not included"),
        (2, "Dimasukkan sebagian / Partially included"),
        (3, "Dimasukkan semua / Fully included"),
    ] + SKIP_CODES
    rightsid = models.IntegerField(primary_key=True)
    kii = models.ForeignKey(KII, on_delete=models.PROTECT)
    usernamel = models.CharField(max_length=255, default=str(NODATA[0]))
    username = models.CharField(max_length=255, default=str(NODATA[0]))
    userrule = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    userspecrulel = models.CharField(max_length=255, default=str(NODATA[0]))
    userspecrule = models.CharField(max_length=255, default=str(NODATA[0]))
    govtsupport = models.PositiveSmallIntegerField(
        choices=KII_GOVT_SUPPORT_CHOICES, default=NODATA[0]
    )
    userrulesinc = models.PositiveSmallIntegerField(
        choices=KII_RULE_INCLUDED_CHOICES, default=NODATA[0]
    )

    def __str__(self):
        return self.userrule


class SpeciesRule(BaseModel):
    sppruleid = models.IntegerField(primary_key=True)
    kii = models.ForeignKey(KII, on_delete=models.PROTECT)
    speciescommonl = models.CharField(max_length=255, default=str(NODATA[0]))
    speciescommon = models.CharField(max_length=255, default=str(NODATA[0]))
    family = models.CharField(max_length=255, default=str(NODATA[0]))
    genus = models.CharField(max_length=255, default=str(NODATA[0]))
    species = models.CharField(max_length=255, default=str(NODATA[0]))
    spprule = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    sppspecificrulel = models.TextField(default=str(NODATA[0]))
    sppspecificrule = models.TextField(default=str(NODATA[0]))

    def __str__(self):
        return self.spprule


class Zone(BaseModel):
    zoneid = models.IntegerField(primary_key=True)
    kii = models.ForeignKey(KII, on_delete=models.PROTECT)
    zonetypel = models.CharField(max_length=255, default=str(NODATA[0]))
    zonetype = models.CharField(max_length=255, default=str(NODATA[0]))
    zonequantity = models.PositiveSmallIntegerField(default=NODATA[0])
    zoneorg = models.PositiveSmallIntegerField(
        choices=YES_NO_CHOICES, default=NODATA[0]
    )
    zonecoord = models.PositiveSmallIntegerField(
        choices=KII_FREQ_CHOICES, default=NODATA[0]
    )

    def __str__(self):
        return self.zonetype


class LkpAssetAssistance(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)


class LkpAssetObtain(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)


class LkpFishTechCategory(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return self.category


class LkpFishTechnique(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255)
    english = models.CharField(max_length=255)
    consolidatedfishtechcategory = models.ForeignKey(
        "LkpFishTechCategory", on_delete=models.SET_NULL, blank=True
    )

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return "{} [{}]".format(self.bahasaindonesia, self.english)


class LkpFreqFishTime(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return "{} [{}]".format(self.bahasaindonesia, self.english)


class LkpGroup(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return "{} [{}]".format(self.bahasaindonesia, self.english)


class LkpLivelihood(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return "{} [{}]".format(self.bahasa, self.english)


class LkpNoneToAllScale(BaseLookupModel):
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
        ordering = ("code",)

    def __str__(self):
        return "{} [{}]".format(self.bahasaindonesia, self.english)


class HouseholdSurveyVersion(BaseLookupModel):
    version = models.CharField(max_length=255)
    notes = models.TextField(blank=True)


    class Meta:
        ordering = ("version",)

    def __str__(self):
        return self.version


class FGDSurveyVersion(BaseLookupModel):
    version = models.CharField(max_length=255)
    notes = models.TextField(blank=True)


    class Meta:
        ordering = ("version",)

    def __str__(self):
        return self.version

class KIISurveyVersion(BaseLookupModel):
    version = models.CharField(max_length=255)
    notes = models.TextField(default=str(NODATA[0]))


    class Meta:
    ordering = ("version",)

    def __str__(self):
    return self.version



class LkpMPANetwork(BaseLookupModel):
    # TODO: What properties should this model have?
    # Done. Please chekc if "__str__(self):" properties needed too
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
    ordering = ("code",)

class LkpSeascape(BaseLookupModel):
    # TODO: What properties should this model have?
    # Done. Please chekc if "__str__(self):" properties needed too
    code = models.IntegerField(primary_key=True)
    bahasaindonesia = models.CharField(max_length=255, blank=True)
    english = models.CharField(max_length=255, blank=True)

    class Meta:
    ordering = ("code",)
